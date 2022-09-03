{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Contracts.OffChain.LockValue(
           LockValueSchema,
           lockValueContract
        )where

import           GHC.Generics
import           Control.Lens.Getter   (view, (^.) )
import qualified Data.Aeson
import           Data.Aeson            hiding (Value)
import qualified Data.Aeson.Types
import qualified Data.Foldable         as Foldable
import           Data.Map              (Map, (!))
import qualified Data.Map              as Map
import qualified Data.List             as List
import           Data.Maybe            (catMaybes, listToMaybe)
import qualified Ledger
import           Ledger                (Value)
import qualified Ledger.Constraints    as Constraints
import qualified Ledger.Constraints.OffChain  (ScriptLookups, otherScript)
import           Prelude      
import           Plutus.V1.Ledger.Api  (Datum (..), 
                                        Redeemer (..),
                                        POSIXTime (..),
                                        TxId (..))
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import           Plutus.Contract
import           Plutus.Contract.Types
import           Playground.Contract
import qualified PlutusTx
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)

import           Plutus.Contracts.OnChain.LockValue (LockValueDatum (..), LockValueParams (..),
                                                     validateLockValueInstance,
                                                    )                       
import           Plutus.Contracts.OffChain.ProofspaceCommon (GError (..), pkhFromHexString, txIdFromHexString) 



data LockValueToCredParams = LockValueToCredParams {
      lvtcpExpireTimeMillis:: Integer,
      lvtcpValue:: Value,
      lvtcpCode:: String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- script acurrentOutAddressddress as hex string
data LockValueToCredResult = LockValueToCredResult {
      lvtcrExpireTimeMillis :: Integer,
      lvtcrTxIdS :: String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


data ClaimLockedValueParams = ClaimLockedValueParams {
      clvTxIdS :: String,
      clvCode :: String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

         
type LockValueSchema =
         Endpoint "lockValueToCred" LockValueToCredParams
     .\/ Endpoint "claimLockedValue"  ClaimLockedValueParams
    

-- tproxy call lockValueToCred from client did
-- and return to caller transactionId, which is packet to
-- credential and send to did as credential
lockValueToCred :: AsContractError e => Promise () LockValueSchema e LockValueToCredResult
lockValueToCred = endpoint @"lockValueToCred" @LockValueToCredParams $ \params -> do
    logInfo @String $ "Lock value to the script"
    let datum = LockValueDatum {
        lvdCodeString = stringToBuiltinByteString (lvtcpCode params)
    }
    let scriptParams = LockValueParams {
        lvpExpireTime = POSIXTime (lvtcpExpireTimeMillis params)
    }
    let validatorInstance = validateLockValueInstance scriptParams
    let scriptAddress = Ledger.scriptAddress $ Scripts.validatorScript validatorInstance
    let txc = Constraints.mustPayToTheScript datum (lvtcpValue params)
    tx <- submitTxConstraints validatorInstance txc
    let txId = Ledger.getCardanoTxId tx
    _ <- awaitTxConfirmed txId
    return $ LockValueToCredResult {
        lvtcrExpireTimeMillis = lvtcpExpireTimeMillis params,
        lvtcrTxIdS = show txId
    }

-- when somebody receive a credential, it can us thse credential to
-- claim a locked value.
claimLockedValue :: Promise () LockValueSchema  GError ()
claimLockedValue = endpoint @"claimLockedValue" @ClaimLockedValueParams $ \params -> do
    txOutRefId <- case (txIdFromHexString (clvTxIdS params)) of
                       Left msg -> throwError (GTextError msg)
                       Right txId -> return txId
    let txOutRef = Ledger.TxOutRef {
        txOutRefId = txOutRefId,
        txOutRefIdx = 0
    }
    -- TOSO:  what is the guarantee that transaction have only one utxo (?)
    mbUtxos <- unspentTxOutFromRef txOutRef
    utxo <- case mbUtxos of
              Just utxo -> return utxo
              Nothing ->  throwError (GTextError "Utxo output already was spent")
    let datum = LockValueDatum {
        lvdCodeString = stringToBuiltinByteString (clvCode params)
    }
    let datumData = PlutusTx.toBuiltinData datum
    let datum = Datum datumData
    myPkh <- ownPaymentPubKeyHash
    let valueToClaim = view Ledger.ciTxOutValue utxo
    validatorHash <- case Ledger.toValidatorHash (view Ledger.ciTxOutAddress utxo) of
                          Just hash -> return hash
                          Nothing -> throwError $ GTextError "Supplied transactoon id is invalid"
    let constraints = (Constraints.mustPayWithDatumToPubKey myPkh datum valueToClaim) <>
                      (Constraints.mustSpendScriptOutput txOutRef (Redeemer datumData))
    tx <-  submitTx constraints
    let txId = Ledger.getCardanoTxId tx
    logInfo @String $ "transaction submitted" <> show txId 
    _ <- awaitTxConfirmed txId
    return ()
    


-- contract itself
lockValueContract :: Contract () LockValueSchema GError ()
lockValueContract = do
    logInfo @String  $ "LockValue contract: waiting select"
    let step:: Promise () LockValueSchema GError (Either LockValueToCredResult ())  = (selectEither lockValueToCred claimLockedValue) 
    (toContract step) >> lockValueContract
