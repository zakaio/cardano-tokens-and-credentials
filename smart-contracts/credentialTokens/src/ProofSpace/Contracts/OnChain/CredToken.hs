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

--
-- 
module ProofSpace.Contracts.OnChain.CredToken(
    CredTokenParams (..),
    credTokenNFTMintingPolicy,
    --credTokenNTTMintingPolicy
) where


import           Ledger
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import           Plutus.V1.Ledger.Api  (Address,
                                        DatumHash (..), Datum (..), 
                                        PubKeyHash, 
                                        POSIXTime,
                                        ScriptContext, 
                                        Validator,
                                        mkValidatorScript)
import qualified Plutus.V1.Ledger.Interval            as Interval
import qualified Plutus.V1.Ledger.TxId                as TxId
import qualified Plutus.V1.Ledger.Value               as Value
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import           PlutusTx.AssocMap     as Map


data CredTokenType = CredTokenNFT -- one-time token
                    |
                     CredTokenNTT -- non-transferable token
                    |
                     CredTypeDAT -- data token (data encoded in token name)
                    |
                     CredTypePTR -- Reference to transaction output is in token name (Vasil)


PlutusTx.makeLift           ''CredTokenType
PlutusTx.unstableMakeIsData ''CredTokenType

data CredTokenParams = CredTokenParams {
    -- Authority who signs a request
    ctpAuthority         :: PubKeyHash,

    -- credential request: request, which is
    -- send to credential api. Each credential request
    -- with token type define a native currency.
    ctpCredentialRequest :: BuiltinByteString,
    ctpType              :: CredTokenType
}

PlutusTx.makeLift ''CredTokenParams


data CredentialRequestDatum = CredentialRequestDatum {
    crdCredentialRequest :: BuiltinByteString,
    crdType              :: CredTokenType
}

PlutusTx.makeLift           ''CredentialRequestDatum
PlutusTx.unstableMakeIsData ''CredentialRequestDatum


{-# INLINABLE credTokenNFTMintingPolicy #-}
credTokenNFTMintingPolicy :: CredTokenParams -> BuiltinData -> BuiltinData -> ()
credTokenNFTMintingPolicy params _ ctxData =
    if (not (txSignedBy txInfo authority)) then
        traceError "Invalid transaction signature"
    else
        case findDatumHash credRequestDatum txInfo of
            Nothing -> traceError("credential request datum is not found")
            Just dh ->
                case find (\x -> (txOutDatumHash (txInInfoResolved x)) == Just dh) inputs of
                    Nothing -> traceError("input with datum hash is not found")
                    Just inInfo ->
                        let 
                            txBytes = TxId.getTxId(txOutRefId (txInInfoOutRef inInfo))
                            tkBytes = Value.unTokenName tokenName
                        in
                            if (txBytes == tkBytes) then
                                ()
                            else
                                traceError "TokenName mismatch"
      where
        ctx = PlutusTx.unsafeFromBuiltinData ctxData
        txInfo = scriptContextTxInfo ctx
        inputs = txInfoInputs txInfo
        authority = ctpAuthority params
        credRequest = ctpCredentialRequest params
        credRequestTypedDatum = CredentialRequestDatum credRequest CredTokenNFT
        credRequestDatum = Datum $ PlutusTx.toBuiltinData credRequestTypedDatum
        tokenName = case Map.lookup (ownCurrencySymbol ctx) (Value.getValue (txInfoMint txInfo)) of
                      Nothing -> traceError("ownCurrencySymbol is not found")
                      Just tokenNames ->
                        case Map.toList tokenNames of
                            [x] -> (fst x)
                            (x:y:xs) -> traceError("token name for minting should be one")


        
        



