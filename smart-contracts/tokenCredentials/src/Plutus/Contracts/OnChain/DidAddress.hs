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
{-# LANGUAGE OverloadedStrings          #-}
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
      SubmitDidDatum (..),
      ClaimDidDatum (..)
    -- * Scripts
    , submitDidValidator
    , claimDidValidator
    , didAddressMintingPolicy
    , didAddressMintingPolicyScript
    -- * Address
    , submitDidAddress
    , submitDidInstance
    , claimDidInstance
    , claimDidAddress
    -- * API
    , didAddressTokenCurrency 
    , checkDidAddressToken
    ) where

import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as C
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Plutus.V1.Ledger.Api  (Address, CurrencySymbol (..), 
                                        DatumHash (..), Datum (..), 
                                        PubKeyHash, ScriptContext, Validator, 
                                        getValue,
                                        MintingPolicy (..), MintingPolicyHash (..),
                                        mkValidatorScript, mkMintingPolicyScript)
import           Plutus.V1.Ledger.Scripts      (Script (..), ScriptHash (..))
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Tx             (ChainIndexTxOut (..))
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V1.Scripts as Scripts
import           PlutusTx.AssocMap     as Map



data SubmitDidDatum = SubmitDidDatum {
      dtSubmitDid :: !BuiltinByteString,
      dtSubmitPkh :: !PubKeyHash
   } 

PlutusTx.unstableMakeIsData ''SubmitDidDatum
PlutusTx.makeLift ''SubmitDidDatum

data ClaimDidDatum = ClaimDidDatum {
      dtClaimDid :: !BuiltinByteString,
      dtClaimPkh :: !PubKeyHash,
      dtClaimCode :: !BuiltinByteString
  } 

PlutusTx.unstableMakeIsData ''ClaimDidDatum
PlutusTx.makeLift ''ClaimDidDatum

data DidAddressParams = DidAddressParams {
      contractOwner :: !PubKeyHash
}

PlutusTx.makeLift ''DidAddressParams


data SubmitDid
instance Scripts.ValidatorTypes SubmitDid where
    type instance RedeemerType SubmitDid = ()
    type instance DatumType SubmitDid = SubmitDidDatum

submitDidInstance :: PubKeyHash -> Scripts.TypedValidator SubmitDid
submitDidInstance pkh = Scripts.mkTypedValidator @SubmitDid
    ($$(PlutusTx.compile [|| validateSubmit ||])
            `PlutusTx.applyCode` PlutusTx.liftCode pkh
    )
    $$(PlutusTx.compile [|| wrap ||]) 
     where
        wrap = Scripts.mkUntypedValidator @SubmitDidDatum @()


-- |  here we allow access for any transaction from contract owner.
-- |  (this is prevent floodign the contract owner by incorrect transactions, which will be
-- |   fail on stage2, which will cost money for owner).
-- |  Note, that we need submitDid be a transaction, to avoid spending for minting did-address token
-- |  In principle,address where person send upfront payment and did can be a non-contract
-- |  address, but personal address, parsed by offchain.
-- |  We keep it here as contract address just to have all addresses in one place.
validateSubmit :: PubKeyHash -> SubmitDidDatum -> () -> ScriptContext -> Bool
validateSubmit ownerPkh datum _ ctx =  
                  Ledger.txSignedBy (Ledger.scriptContextTxInfo ctx) ownerPkh 
                  

submitDidValidator :: PubKeyHash -> Validator 
submitDidValidator pkh = Scripts.validatorScript (submitDidInstance pkh)

-- | The address of the contract (the hash of its validator script)
submitDidAddress :: PubKeyHash -> Address
submitDidAddress pkh = Ledger.scriptAddress (submitDidValidator pkh)


-- submitDid -> [external transaction by contract owner, mint] -> claimDid -> [claiming transaction]
--

-- ! Minting Policy
--

{-# INLINABLE didAddressMintingPolicy #-}
didAddressMintingPolicy :: PubKeyHash -> PubKeyHash -> BuiltinByteString -> BuiltinData -> BuiltinData -> ()
didAddressMintingPolicy  contractOwner  didOwner did  _  ctxData =
            if (not (Ledger.txSignedBy txInfo contractOwner)) then
              traceError  "invalid signature"
            else 
              (case find (\x -> (snd x) == didOwnerDid) (Ledger.txInfoData txInfo) of
                 Nothing -> error () 
                 Just correctDatum -> ()
              )
              where
                  ctx = PlutusTx.unsafeFromBuiltinData ctxData
                  txInfo = (Ledger.scriptContextTxInfo ctx)
                  didOwnerDid = Datum( (PlutusTx.toBuiltinData SubmitDidDatum {
                    dtSubmitDid = did,
                    dtSubmitPkh = didOwner
                  }))
           

{-# INLINABLE didAddressMintingPolicyScript #-}
didAddressMintingPolicyScript :: PubKeyHash -> PubKeyHash -> BuiltinByteString -> Scripts.MintingPolicy
didAddressMintingPolicyScript contractOwner didOwner did =
           mkMintingPolicyScript
             ($$(PlutusTx.compile [|| didAddressMintingPolicy ||])
                   `PlutusTx.applyCode` PlutusTx.liftCode contractOwner
                   `PlutusTx.applyCode` PlutusTx.liftCode didOwner
                   `PlutusTx.applyCode` PlutusTx.liftCode did
             )


-- this will not work onchain, because it's impossible for now to calculate script hash from script 
{-# INLINABLE didAddressTokenCurrency #-}
didAddressTokenCurrency :: PubKeyHash -> PubKeyHash -> BuiltinByteString -> CurrencySymbol
didAddressTokenCurrency    contractOwner  didOwner did  =
                CurrencySymbol (getScriptHash sh)            
             where
               sh = Scripts.scriptHash script
               script = getMintingPolicy (didAddressMintingPolicyScript contractOwner didOwner did)


{-# INLINABLE checkDidAddressToken #-}
checkDidAddressToken :: PubKeyHash -> PubKeyHash -> BuiltinByteString -> CurrencySymbol -> Bool
checkDidAddressToken    contractOwner  didOwner did  currencySymbol =
               (didAddressTokenCurrency contractOwner didOwner did) == currencySymbol                



data ClaimDid
instance Scripts.ValidatorTypes ClaimDid where
    type instance RedeemerType ClaimDid = ()
    type instance DatumType ClaimDid = ClaimDidDatum


-- here token and submitCode are sent by offchain part.
-- an author of transaction can receive minted token if he knows code and address
{-# INLINABLE validateClaimDid #-}
validateClaimDid:: PubKeyHash -> ClaimDidDatum -> () -> ScriptContext -> Bool 
validateClaimDid contractOwner datum _ ctx = 
                     Ledger.txSignedBy txInfo (dtClaimPkh datum) -- &&
                     --isJust (Map.lookup daCurrency (getValue (Ledger.txInfoFee txInfo)))
                     where
                        txInfo = (Ledger.scriptContextTxInfo ctx)
                     --   contrat owner is not needed now because check for currency is diabled.
                     --   daCurrency = didAddressTokenCurrency contractOwner
                     --                                        (dtClaimPkh datum)
                     --                                        (dtClaimDid datum)


claimDidInstance :: PubKeyHash -> Scripts.TypedValidator ClaimDid
claimDidInstance contractOwner = Scripts.mkTypedValidator @ClaimDid
    ($$(PlutusTx.compile [|| validateClaimDid ||])
            `PlutusTx.applyCode` PlutusTx.liftCode contractOwner
    )
    $$(PlutusTx.compile [|| wrap ||]) 
     where
        wrap = Scripts.mkUntypedValidator @ClaimDidDatum @()

claimDidValidator :: PubKeyHash -> Validator 
claimDidValidator contractOwner = Scripts.validatorScript (claimDidInstance contractOwner)

claimDidAddress :: PubKeyHash -> Address
claimDidAddress contractOwner = Ledger.scriptAddress (claimDidValidator contractOwner)

