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

-- accepting payment and issuing credential about this.
module ProofSpace.Contracts.OffChain.PaymentCred(
           PaymentCredSchema,
           paymentCredContract
        )where

import           GHC.Generics
import           Control.Lens.Getter   (view, (^.) )
import           Crypto.Hash
import qualified Data.ByteString       as BS
import Data.ByteString.UTF8            as BSU   
import qualified Data.Aeson
import           Data.Aeson            hiding (Value)
import qualified Data.Aeson.Types
import qualified Data.Foldable         as Foldable
import           Data.Map              (Map, (!))
import qualified Data.Map              as Map
import qualified Data.List             as List
import           Data.Maybe            (catMaybes, listToMaybe)
import qualified Ledger
import           Ledger                (Value, PaymentPubKeyHash (..))
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

import           ProofSpace.Contracts.OnChain.LockValue (LockValueDatum (..), LockValueParams (..),
                                                     validateLockValueInstance,
                                                    )                       
import           ProofSpace.Contracts.OffChain.ProofspaceCommon (GError (..), 
                                                                 pkhFromHexStringM, 
                                                                 txIdFromHexString) 



data PaymentCredParams = PaymentCredParams {
      ppDid        :: !String,
      ppValue      :: !Value,
      ppPkh        :: !String,
      ppNonce      :: !String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


data PaymentCredResult = PaymentCredResult {
      prTxIdS :: !String,
      prTxIdx :: !Integer,
      prVerifyHash :: !String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

type PaymentCredSchema =
         Endpoint "paymentCred" PaymentCredParams
   

--
-- actually just do payment.
-- should 
--
paymentCred :: Promise () PaymentCredSchema GError PaymentCredResult
paymentCred = endpoint @"paymentCred" @PaymentCredParams $ \params -> do
    logInfo @String $ "Do payment for address "
    pkh  <- pkhFromHexStringM (ppPkh params)
    let txc = Constraints.mustPayToPubKey (PaymentPubKeyHash pkh) (ppValue params)
    --WARN:  here we can't send MetaData
    --TODO:  reference input when Vasil will be available.
    let did = ppDid params 
    let nonce = ppNonce params 
    let verifyHash::(Digest SHA3_256) = (hash (BSU.fromString (did <> nonce)))
    tx <- submitTx txc 
    let txId = Ledger.getCardanoTxId tx
    _ <- awaitTxConfirmed txId
    --tell $ Just (ppDid params)
    return $ PaymentCredResult {
        prTxIdS = show txId,
        prTxIdx = 0,
        prVerifyHash = show verifyHash
    }


--verifyPayment :: AsContractError e => Promise (Maybe String) PaymentCredSchema e PaymentCredResult
--verifyPayment :: = endpoint @"paymentCred" @PaymentCredParams $ \params -> do

paymentCredContract :: Contract () PaymentCredSchema GError ()
paymentCredContract = do
    logInfo @String  $ "Payment contract: waiting select"
    (toContract paymentCred) >> paymentCredContract
