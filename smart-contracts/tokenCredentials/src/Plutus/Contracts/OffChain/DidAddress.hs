{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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


-- | Receive 
module Plutus.Contracts.OffChain.DidAddress
    ( submitDid
    , claimDid
    , didAddress
    , DidAddressSchema
    , SubmitDidParams
    , ClaimDidParams
    ) where

import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Plutus.V1.Ledger.Api  (Address, ScriptContext, Validator, Value, Datum(Datum))
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Tx             (ChainIndexTxOut (..))
import           Playground.Contract
import           Plutus.Contract
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import           Plutus.Contracts.OnChain.DidAddress 
import           Plutus.Contracts.OnChain.DidAddress (DidDatum (..))
--import qualified Network.Http.Client  as HttpClient


type DidAddressSchema =
        Endpoint "submitDid" SubmitDidParams
        .\/ Endpoint "claimDid" ClaimDidParams

-- | Parameters for the "submitDid" endpoint
data SubmitDidParams = SubmitDidParams
    { 
      submittedDid :: Haskell.String
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "claimDid" endpoint
data ClaimDidParams = ClaimDidParams
    { claimedDid :: Haskell.String,
      claimCode :: Haskell.String 
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)



-- | The "submitDid" contract endpoint. See note [Contract endpoints]
-- | Actually, this code will be implemented not in pub,
-- | Because it should come from user address, so submit did endpoint
-- | Is unaviable for him.
-- | But we will leave imlementation here
submitDid :: AsContractError e => Promise () DidAddressSchema e ()
submitDid = endpoint @"submitDid" @SubmitDidParams $ \(SubmitDidParams did) -> do
    let code = "1234" -- | oTODO: generate random code and send to DID
    --tell code
    logInfo @Haskell.String $ "Submit " <> Haskell.show did <> " to the script"
    --let minAmount = Ada.lovelaceValueOf 1
    --let tx         = Constraints.mustPayToTheScript (convertDidDatum did code) minAmount
    --void (submitTxConstraints didAddressInstance tx)

convertDidDatum :: Haskell.String -> Haskell.String -> DidDatum
convertDidDatum did code = (DidDatum (stringToBuiltinByteString did) (stringToBuiltinByteString code))


-- | The "claimDid" contract endpoint. See note [Contract endpoints]
-- | This code can be called from out part, when we receive from 
claimDid :: AsContractError e => Promise () DidAddressSchema e ()
claimDid = endpoint @"claimDid" @ClaimDidParams $ \(ClaimDidParams did code) -> do
    -- logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq didAddressAddress (Ada.lovelaceValueOf 1)

    --
    --let redeemer = hashString code
    --    tx       = collectFromScript utxos redeemer

    -- In a real use-case, we would not submit the transaction if we know that code is invalie
    -- wrong.
    logInfo @Haskell.String "Submitting transaction to claim did"
    let minAmount = Ada.lovelaceValueOf 1
    let didDatum  = convertDidDatum did code
    let tx = Constraints.mustPayToTheScript didDatum minAmount
    void (submitTxConstraintsSpending didAddressInstance utxos tx)

didAddress :: AsContractError e => Contract () DidAddressSchema e ()
didAddress = do
    logInfo @Haskell.String "Waiting for guess or lock endpoint..."
    selectList [submitDid, claimDid] >> didAddress

