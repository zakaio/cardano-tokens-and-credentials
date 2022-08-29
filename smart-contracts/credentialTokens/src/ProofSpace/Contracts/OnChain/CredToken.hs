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
    credTokenNFTMintingPolicyScript,
    credTokenNTTMintingPolicy,
    credTokenNTTMintingPolicyScript,
    credTokenDATMintingPolicy,
    credTokenDATMintingPolicyScript,
    validateNTT
) where


import           Ledger
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import           Plutus.V1.Ledger.Api  (Datum (..), 
                                        ScriptContext, 
                                        Validator,
                                        mkValidatorScript,
                                        mkMintingPolicyScript)
import qualified Plutus.V1.Ledger.TxId                as TxId
import qualified Plutus.V1.Ledger.Value               as Value
import qualified Plutus.V1.Ledger.Crypto              as Crypto
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import qualified PlutusTx.AssocMap                    as Map


data CredTokenType = CredTokenNFT -- one-time token
                    |
                     CredTokenNTT -- non-transferable token
                    |
                     CredTokenDAT -- data token (data encoded in token name)
                    |
                     CredTokenPTR -- Reference to transaction output is in token name (Vasil)


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


{-# INLINABLE credTokenGenMintingPolicy #-}
credTokenGenMintingPolicy :: CredTokenParams -> (ScriptContext -> TxInInfo -> ()) -> BuiltinData -> BuiltinData -> ()
credTokenGenMintingPolicy params typeSpecificCheck _ ctxData =
    if (not (txSignedBy txInfo authority)) then
        traceError "Invalid transaction signature"
    else
        case findDatumHash credRequestDatum txInfo of
            Nothing -> traceError("credential request datum is not found")
            Just dh ->
                case find (\x -> (txOutDatumHash (txInInfoResolved x)) == Just dh) inputs of
                    Nothing -> traceError("input with datum hash is not found")
                    Just inInfo ->
                         typeSpecificCheck ctx inInfo
      where
        ctx = PlutusTx.unsafeFromBuiltinData ctxData
        txInfo = scriptContextTxInfo ctx
        inputs = txInfoInputs txInfo
        authority = ctpAuthority params
        credRequest = ctpCredentialRequest params
        credRequestTypedDatum = CredentialRequestDatum credRequest (ctpType params)
        credRequestDatum = Datum $ PlutusTx.toBuiltinData credRequestTypedDatum
        tokenName = case Map.lookup (ownCurrencySymbol ctx) (Value.getValue (txInfoMint txInfo)) of
                      Nothing -> traceError("ownCurrencySymbol is not found")
                      Just tokenNames ->
                        case Map.toList tokenNames of
                            [x] -> (fst x)
                            (x:y:xs) -> traceError("token name for minting should be one")



-- here we have unique token name, any amount
-- useful in situation, where we have quasi-unique credential request
-- and want to have an answer as nft token
{-# INLINABLE credTokenNFTMintingPolicy #-}
credTokenNFTMintingPolicy :: CredTokenParams -> BuiltinData -> BuiltinData -> ()
credTokenNFTMintingPolicy params redeemper ctxData =
    credTokenGenMintingPolicy params checkNFT redeemper ctxData 
    where
        checkNFT:: ScriptContext -> TxInInfo -> ()
        checkNFT ctx input =
            let 
                tokenName = mintedTokenName ctx
                txBytes = TxId.getTxId(txOutRefId (txInInfoOutRef input))
                tkBytes = Value.unTokenName tokenName
                        in
                            if (txBytes == tkBytes) then
                                ()
                            else
                                traceError "TokenName mismatch"


{-# INLINABLE mintedTokenName #-}
mintedTokenName :: ScriptContext -> TokenName
mintedTokenName ctx =
    case Map.lookup (ownCurrencySymbol ctx) (Value.getValue (txInfoMint txInfo)) of
        Nothing -> traceError("ownCurrencySymbol is not found")
        Just tokenNames ->
                case Map.toList tokenNames of
                    [x] -> (fst x)
                    (x:y:xs) -> traceError("token name for minting should be one")
      where
        txInfo = scriptContextTxInfo ctx



-- NTT - No transferrable token
-- here we have token name which should be the same as address
-- this address can be minted only if transaction send to him.
-- then any smart-comtract can check the validity of token
-- by comaring token name and address of input from which token
-- was send to contract.
-- [we will mint this token as additional step after claimCode]
{-# INLINABLE credTokenNTTMintingPolicy #-}
credTokenNTTMintingPolicy :: CredTokenParams -> BuiltinData -> BuiltinData -> ()
credTokenNTTMintingPolicy params redeemer ctxData =
    credTokenGenMintingPolicy params checkNTT redeemer ctxData
        where
            checkNTT :: ScriptContext -> TxInInfo -> ()
            checkNTT ctx input =
                case mintedOutputs of
                    [] -> traceError "no minted outputs"
                    [x] ->
                        case Ledger.toPubKeyHash (txOutAddress x) of 
                            Nothing -> traceError "NTT Token can't be send to script"
                            Just pkh ->
                                let 
                                    tkBytes = Value.unTokenName tokenName
                                    pkhBytes = Crypto.getPubKeyHash pkh
                                in
                                    if (tkBytes /= pkhBytes) then 
                                        traceError("token name for other token")
                                    else    
                                        ()
                    (x:xs) -> traceError "too many input outputs"    
                where    
                    txInfo = scriptContextTxInfo ctx
                    txOutputs = txInfoOutputs txInfo
                    mintedOutputs = filter (\x -> elem (ownCurrencySymbol ctx) 
                                           (Value.symbols (txOutValue x)) ) txOutputs
                    tokenName = mintedTokenName ctx


validateNTT :: CurrencySymbol -> ScriptContext -> Bool
validateNTT  credReqSym ctx =
     (not (null myOutputs))
     &&
     (all (\x-> 
          case Ledger.toPubKeyHash (txOutAddress x) of
            Nothing -> False
            Just pkh ->
                let xValueMap = Value.getValue (txOutValue x)
                in
                    case Map.lookup credReqSym xValueMap of
                        Nothing -> False
                        Just tokens -> 
                            all (\tkn ->
                                 (Crypto.getPubKeyHash pkh) == (Value.unTokenName tkn) )
                              (Map.keys tokens) 
          ) 
        myOutputs) 
    where
        txInfo = scriptContextTxInfo ctx
        outputs = txInfoOutputs txInfo
        myOutputs = filter (\x -> elem credReqSym 
                                        (Value.symbols (txOutValue x)) ) outputs
        


-- DAT - token with data
{-# INLINABLE credTokenDATMintingPolicy #-}
credTokenDATMintingPolicy :: CredTokenParams -> BuiltinData -> BuiltinData -> ()
credTokenDATMintingPolicy params redeemper ctxData =
    credTokenGenMintingPolicy params checkDAT redeemper ctxData 
      where
        checkDAT :: ScriptContext -> TxInInfo -> ()
        checkDAT _ _ = ()



-- PTR - token with pointer to reference input
{-# INLINABLE credTokenPTRMintingPolicy #-}
credTokenPTRMintingPolicy :: CredTokenParams -> BuiltinData -> BuiltinData -> ()
credTokenPTRMintingPolicy _ _ _ =
    traceError("will be implemented with plutus V2")


--- can't specify fun to be inlinanble.
--- {-# INLINABLE credTokenGenMintingPolicyScript #-}
--- credTokenGenMintingPolicyScript ::  CredTokenParams -> (CredTokenParams -> BuiltinData -> BuiltinData -> () ) ->  Scripts.MintingPolicy
--- credTokenGenMintingPolicyScript params fun =
---    mkMintingPolicyScript
---             ($$(PlutusTx.compile [|| fun ||])
---                   `PlutusTx.applyCode` PlutusTx.liftCode params
---             )


{-# INLINABLE credTokenNFTMintingPolicyScript #-}
credTokenNFTMintingPolicyScript ::  CredTokenParams ->  Scripts.MintingPolicy
credTokenNFTMintingPolicyScript params =
           mkMintingPolicyScript
             ($$(PlutusTx.compile [|| credTokenNFTMintingPolicy ||])
                   `PlutusTx.applyCode` PlutusTx.liftCode params
             )



{-# INLINABLE credTokenNTTMintingPolicyScript #-}
credTokenNTTMintingPolicyScript ::  CredTokenParams -> Scripts.MintingPolicy
credTokenNTTMintingPolicyScript params =
           mkMintingPolicyScript
             ($$(PlutusTx.compile [|| credTokenNTTMintingPolicy ||])
                   `PlutusTx.applyCode` PlutusTx.liftCode params
             )

{-# INLINABLE credTokenDATMintingPolicyScript #-}
credTokenDATMintingPolicyScript ::  CredTokenParams -> Scripts.MintingPolicy
credTokenDATMintingPolicyScript params =
           mkMintingPolicyScript
             ($$(PlutusTx.compile [|| credTokenDATMintingPolicy ||])
                   `PlutusTx.applyCode` PlutusTx.liftCode params
             )





