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
-- | for Plutus.V2
module ProofSpace.Contracts.OnChain.DidAddressV2
    ( 
      DidDatum (..)
    -- * Scripts
    , didAddressValidator
    , hashString
    -- * Address
    , didAddressAddress
    ) where

import qualified Data.ByteString.Char8 as C
import           Plutus.V2.Ledger.Api  (Address, ScriptContext, Validator, mkValidatorScript)
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Tx             (ChainIndexTxOut (..))
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell
--import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
--import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts


data SubmitDidDatum = SubmitDidDatum {
      dtSubmidDid :: BuiltinByteString,
      dtSubmitCode :: BuiltinByteString,
      dtSubmitPkh :: PubKeyHash
   } deriving anyclass (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)


PlutusTx.makeLift ''SubmitDidDatum

data DidDatum = DidDatum {
      did :: !BuiltinByteString,
      code :: !BuiltinByteString
   } deriving anyclass (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''DidDatum

data DidOwnerDatum = DidOwnerDatum {
      dtDidOwnerDid :: !BuiltinByteString,
      dtDidOwnerPkh :: !PubKeyHash
   } deriving anyclass (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''DidOwnerDatum

data DidAddressParams = DidAddressParams {
      contractOwner :: PubKeyHash,
      publishPrice :: Ledger.Value,
      minContractPrice :: Integer
}

PlutusTx.makeLift ''DidAddressParams



-- use untyped interface for V2 because typed yet not available.
validateSubmitDidUntyped :: DidAddressParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
validateSubmitDidUntyped params datum redeemer context =
    validateSubmitDidATyped params
                            (PlutusTx.unsafeFromBuiltinData datum) 
                            (PlutusTx.unsafeFromBuiltinData redeemer) 
                            (PlutusTx.unsafeFromBuiltinData context)
     
validateSubmitDidTyped :: DidAddressParams -> DidDatum -> () -> ScriptContext -> ()
validateSubmitAddressTyped patams datum _ context = 
                            if isSignedByOwner then () else error()
                            where
                              isSignedByOwner = Ledger.txSignedBy 
                                                      (Ledger.scriptContextTxInfo ctx) 
                                                      (contractOwner params)




submitDidValidator :: DidAddressParams -> Validator
submitDidValidator params = mkValidatorScript $$(PlutusTx.compile [|| validateDidAddressUntyped ||])


submitDidAddress :: DidAddressParams -> Address
submitDidAddress params = Ledger.scriptAddress (didAddressValidator params)

