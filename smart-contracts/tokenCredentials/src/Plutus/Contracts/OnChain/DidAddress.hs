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
module Plutus.Contracts.OnChain.DidAddress
    ( 
      DidDatum (..)
    , DidAddress
    -- * Scripts
    , didAddressValidator
    , hashString
    -- * Address
    , didAddressAddress
    , didAddressInstance
    ) where

import qualified Data.ByteString.Char8 as C
import           Plutus.V1.Ledger.Api  (Address, ScriptContext, Validator)
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

data DidAddress
instance Scripts.ValidatorTypes DidAddress where
    type instance RedeemerType DidAddress = HashedString
    type instance DatumType DidAddress = DidDatum

didAddressInstance :: Scripts.TypedValidator DidAddress
didAddressInstance = Scripts.mkTypedValidator @DidAddress
    $$(PlutusTx.compile [|| validateClaim ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @DidDatum @HashedString

hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

validateClaim :: DidDatum -> HashedString -> ScriptContext -> Bool
validateClaim didDatum (HashedString hs) _ =  hs == sha2_256 (code didDatum)

-- | The validator script 
didAddressValidator :: Validator
didAddressValidator = Scripts.validatorScript didAddressInstance

-- | The address of the game (the hash of its validator script)
didAddressAddress :: Address
didAddressAddress = Ledger.scriptAddress didAddressValidator

