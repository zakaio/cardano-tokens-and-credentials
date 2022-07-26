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
module Plutus.Contracts.DidAddress
    ( submitDid
    , claimDid
    , didAddress
    , DidAddressSchema
    , didAddressAddress
    , SubmitDidParams
    , ClaimDidParams
    -- * Scripts
    , didAddressValidator
    , hashString
    -- * Address
    , didAddressAddress
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

newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype CodeString = CodeString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''CodeString

data DidDatum = DidDatum {
      did :: BuiltinByteString,
      code :: BuiltinByteString 
   } deriving anyclass (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)


PlutusTx.makeLift ''DidDatum

type DidAddressSchema =
        Endpoint "submitDid" SubmitDidParams
        .\/ Endpoint "claimDid" ClaimDidParams

data DidAddress
instance Scripts.ValidatorTypes DidAddress where
    type instance RedeemerType DidAddress = HashedString
    type instance DatumType DidAddress = DidDatum

didAddressInstance :: Scripts.TypedValidator DidAddress
didAddressInstance = Scripts.mkTypedValidator @DidAddress
    $$(PlutusTx.compile [|| validateClaim ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @DidDatum @HashedString

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
validateClaim :: DidDatum -> HashedString -> ScriptContext -> Bool
validateClaim didDatum (HashedString hs) _ =  hs == sha2_256 (code didDatum)

-- | The validator script 
didAddressValidator :: Validator
didAddressValidator = Scripts.validatorScript didAddressInstance

-- | The address of the game (the hash of its validator script)
didAddressAddress :: Address
didAddressAddress = Ledger.scriptAddress didAddressValidator

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
submitDid :: AsContractError e => Promise () DidAddressSchema e ()
submitDid = endpoint @"submitDid" @SubmitDidParams $ \(SubmitDidParams did) -> do
    logInfo @Haskell.String $ "Submit " <> Haskell.show did <> " to the script"
    let code = "1234" -- | TODO: generate random code and send to DID
    let minAmount = Ada.lovelaceValueOf 1
    let tx         = Constraints.mustPayToTheScript (convertDidDatum did code) minAmount
    void (submitTxConstraints didAddressInstance tx)

convertDidDatum :: Haskell.String -> Haskell.String -> DidDatum
convertDidDatum did code = (DidDatum (stringToBuiltinByteString did) (stringToBuiltinByteString code))


-- | The "claimDid" contract endpoint. See note [Contract endpoints]
claimDid :: AsContractError e => Promise () DidAddressSchema e ()
claimDid = endpoint @"claimDid" @ClaimDidParams $ \(ClaimDidParams claimedDid code) -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq didAddressAddress (Ada.lovelaceValueOf 1)

    let redeemer = hashString code
        tx       = collectFromScript utxos redeemer

    -- In a real use-case, we would not submit the transaction if we know that code is invalie
    -- wrong.
    logInfo @Haskell.String "Submitting transaction to claim did"
    void (submitTxConstraintsSpending didAddressInstance utxos tx)

didAddress :: AsContractError e => Contract () DidAddressSchema e ()
didAddress = do
    logInfo @Haskell.String "Waiting for guess or lock endpoint..."
    selectList [submitDid, claimDid] >> didAddress

