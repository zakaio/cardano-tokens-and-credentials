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

module ProofSpace.Contracts.OffChain.CredToken
    ( 
      submitCredTokenDATRequest
      , mintDATCredToken
      , claimDATCredToken
      , getCredRequestCurrencySymbol
      , submitNTTCodeRequestForDid
      , mintNTTCredToken
    --, CredTokenSchema (..)
    , CredTokenServiceEndpoints (..)
    , CredTokenUserEndpoints (..)
    , ContractParams (..)
    ) where

import           Control.Monad         (void)
import qualified Control.Lens          as Lens
import           Control.Lens.Getter   (view, (^.) )
import qualified Data.Aeson
import           Data.Aeson            hiding (Value)
import qualified Data.Aeson.Types
import qualified Data.Foldable         as Foldable
import qualified Data.Map              as Map
import qualified Data.List             as List
import           Data.Maybe          
import           Data.Text             (Text (..), pack)
import           Plutus.V1.Ledger.Api  (Address, ScriptContext, Validator, 
                                        Value (..), 
                                        Datum(Datum), DatumHash(DatumHash), PubKeyHash,
                                        Redeemer (Redeemer),
                                        BuiltinByteString,
                                        CurrencySymbol (..),
                                        getMintingPolicy
                                        )
import qualified Plutus.V1.Ledger.Address as Address
import qualified Plutus.V1.Ledger.Value as Value
import qualified Ledger
import           Ledger                (PaymentPubKeyHash (..), unPaymentPubKeyHash, 
                                       CurrencySymbol (..),
                                       txOutRefId, txOutRefIdx,
                                       toPubKeyHash)
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Constraints.OffChain (ScriptLookups,mintingPolicy,otherScript,unspentOutputs, generalise)
import           Ledger.Tx             (ChainIndexTxOut (..), ciTxOutDatum, ciTxOutValue, ciTxOutAddress)
import           Playground.Contract
import           Plutus.Contract
import           Plutus.Contract       (ContractError (..))
import qualified PlutusTx
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import           PlutusTx.Builtins       (sha2_256, appendByteString)
import           PlutusTx.Lattice        ( (/\) )
import qualified PlutusTx.Numeric       as PlutusTxNumeric
import qualified PlutusTx.AssocMap      as AssocMap 
import           Prelude             
import qualified Plutus.Script.Utils.V1.Scripts as UtilsScripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as TScripts

import           Plutus.Contracts.OffChain.ProofspaceCommon (GError (..), 
                                                             pkhFromHexString,
                                                             pkhFromHexStringM,
                                                             txIdFromHexStringM,
                                                             findOutputToPubKeyHash,
                                                             findOutputToPubKeyHashM,
                                                             findOutputToValidatorHash,
                                                             tokenNameFromHexStringM,
                                                             ) 
import           ProofSpace.Contracts.OnChain.CredToken(
                                    CredTokenType (..),
                                    CredTokenParams (..),
                                    CredentialMintClaimDatum (..),
                                    credTokenNFTMintingPolicy,
                                    credTokenNFTMintingPolicyScript,
                                    credTokenDATMintingPolicy,
                                    credTokenDATMintingPolicyScript,
                                    credTokenNTTMintingPolicy,
                                    credTokenNTTMintingPolicyScript,
                                    claimCredTokenValidator,
                                    claimCredTokenInstance,
                                    ClaimCredToken (..)
                                             )


data ContractParams = ContractParams {
        owner :: PubKeyHash,
        publishPrice :: Value,
        claimBackPrice :: Value,
        credentialAmount :: Integer,
        nttPkh :: PubKeyHash
     }
     deriving stock (Eq, Show, Generic)
     deriving anyclass (FromJSON, ToJSON)


data SubmitCredTokenDATRequestParams = SubmitCredTokenDATRequestParams {        
        sctdrpCredentialRequest:: !String,
        sctdrpTokenName        :: !String,
        sctdrpDid              :: !String,
        sctdrpServicePkh       :: !String,
        sctdrpAmount           :: !Value
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data SubmitCredTokenDATDatum = SubmitCredTokenDATDatum {
    sctddCredentialRequest:: !BuiltinByteString,
    sctddTokenName        :: !TokenName,
    sctddDid              :: !BuiltinByteString
}

PlutusTx.makeLift           ''SubmitCredTokenDATDatum
PlutusTx.unstableMakeIsData ''SubmitCredTokenDATDatum

data SubmitCredTokenDATRequestResult = SubmitCredTokenDATRequestResult {        
        sctdrrTxId             :: !String,
        sctdrrOutInd           :: !Integer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


data MintCredTokenDATRequestParams = MintCredTokenDATRequestParams {
        mctdrpCredentialRequest:: !String,
        mctdrpTokenName        :: !String,
        mctdrpDid              :: !String,
        mctdrpServicePkh       :: !String,
        mctdrpCode             :: !String,
        mctdrpMintAmount       :: !Integer,
        mctdrpServicePrice     :: !Value,
        mctdrpSubmitTxId       :: !String,
        mctdrpSubmitTxIdx      :: !Integer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


data MintCredTokenDATRequestResult = MintCredTokenDATRequestResult {
        mctdrrTxId :: !String,
        mctdrrTxIdx :: !Integer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


data ClaimCredTokenDATParams = ClaimCredTokenDATParams {
        cctpCredentialRequest :: !String,
        cctpTokenName :: !String,
        cctpCode :: !String,
        cctpTxId:: !String,
        cctpTxIdx:: !Integer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)



--data MintCredTokenParams = MintCredTokenParams {
--        mctpTxId :: !String,
--        mctpTxInd :: !Integer,
--        mctpCredentialRequest :: !String
--    }
--    deriving stock (Eq, Show, Generic)
--    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data GetCredRequestCurrencySymbolParams = GetCredRequestCurrencySymbolParams {
        gcrcspCredRequest   :: !String,
        gcrcspAuthorityPkh  :: !String,
        gcrcspCredType      :: !String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data SubmitNTTCodeRequestForDidParams = SubmitNTTCodeRequestForDidParams {
        sncrfdDid        :: !String,
        sncrfDidPkh      :: !String, -- ?? are we need this
        sncrfNonce       :: !String, -- TODO
        scnrfServicePrice    :: !Value   -- TODO if we think that service should receive a fee for NTT token
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


data TxOutResult = TxOutResult {
        torId  :: !String,
        torIdx :: !Integer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


data MintCredTokenNTTParams = MintCredTokenNTTParams {
        mctnpDid :: !String,
        mctnpNonce :: !String,
        mctnpCredentialRequest :: !String,
        mctnpMintAmount :: !Integer,
        mctnpServicePkh :: !String,
        mctnpServicePrice :: !Value,
        mctnpSubmitTxId :: !String,
        mctnpSubmitTxIdx :: !Integer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


type CredTokenUserEndpoints =
         Endpoint "submitCredTokenDATRequest" SubmitCredTokenDATRequestParams
     .\/ Endpoint "claimDATCredToken"  ClaimCredTokenDATParams
     .\/ Endpoint "submitNTTCodeRequestForDid" SubmitNTTCodeRequestForDidParams


type CredTokenServiceEndpoints =
         Endpoint "mintDATCredToken"  MintCredTokenDATRequestParams
     .\/ Endpoint "mintNTTCredToken" MintCredTokenNTTParams    
     .\/ Endpoint "getCredRequestCurrencySymbol"   GetCredRequestCurrencySymbolParams


-- |  
submitCredTokenDATRequest :: Promise () CredTokenUserEndpoints GError SubmitCredTokenDATRequestResult
submitCredTokenDATRequest = endpoint @"submitCredTokenDATRequest" @SubmitCredTokenDATRequestParams $ \params -> do
    logInfo @String $ "Submit credential DAT "
    pkh <- (fmap unPaymentPubKeyHash ownPaymentPubKeyHash)
    let binCredentialRequest = stringToBuiltinByteString (sctdrpCredentialRequest params)
    let tokenName = TokenName (stringToBuiltinByteString (sctdrpTokenName params))
    let binDid =  stringToBuiltinByteString (sctdrpDid params)
    authority <- case pkhFromHexString (sctdrpServicePkh params) of
                        Left msg -> throwError (GTextError msg)
                        Right value -> return value 
    let submitTypedDatum = SubmitCredTokenDATDatum {
        sctddCredentialRequest = binCredentialRequest,
        sctddTokenName = tokenName,
        sctddDid = binDid
    }
    let submitDatum = Datum (PlutusTx.toBuiltinData submitTypedDatum)
    let payConstraint = Constraints.mustPayWithDatumToPubKey (PaymentPubKeyHash authority) submitDatum (sctdrpAmount params)
    tx <- submitTx payConstraint
    output <- case findOutputToPubKeyHash authority tx of
                   Just output -> return output
                   Nothing -> throwError (GTextError "our output is not foun in just sbbmitted transaction")
    return $ SubmitCredTokenDATRequestResult {
        sctdrrTxId = show (txOutRefId output),
        sctdrrOutInd = (txOutRefIdx output)
    }
    


mintDATCredToken :: ContractParams -> Promise () CredTokenServiceEndpoints GError MintCredTokenDATRequestResult 
mintDATCredToken contractParams =  endpoint @"mintDATCredToken" @MintCredTokenDATRequestParams $ \params -> do
    let binCredentialRequest = stringToBuiltinByteString (mctdrpCredentialRequest params)
    authority <- pkhFromHexStringM (mctdrpServicePkh params) 
    let onchainMintParams = CredTokenParams {
        ctpAuthority = authority,
        ctpCredentialRequest = binCredentialRequest,
        ctpType = CredTokenDAT
    }
    let datMintingPolicyHash = UtilsScripts.mintingPolicyHash (credTokenDATMintingPolicyScript onchainMintParams)
    let datMintCurrencySymbol = Value.mpsSymbol datMintingPolicyHash
    let binCredentialRequest = stringToBuiltinByteString (mctdrpCredentialRequest params)
    tokenName <- tokenNameFromHexStringM (mctdrpTokenName params)
    let mintAmount = mctdrpMintAmount params
    let mintConstraint = Constraints.mustMintCurrency datMintingPolicyHash tokenName mintAmount 
    let typedMintDatum = CredentialMintClaimDatum {
        cmcdCredentialRequest = binCredentialRequest,
        cmcdType = CredTokenDAT,
        cmcdCode = stringToBuiltinByteString (mctdrpCode params) 
    }
    let mintValue = Value (AssocMap.singleton datMintCurrencySymbol (AssocMap.singleton tokenName mintAmount))
    let claimValue = mintValue <> (claimBackPrice contractParams)
    let mintDatum = Datum $ PlutusTx.toBuiltinData typedMintDatum
    let claimValidatorHash = UtilsScripts.validatorHash (claimCredTokenValidator onchainMintParams)
    let claimPrice = claimBackPrice contractParams
    let claimPayConstraint = Constraints.mustPayToOtherScript claimValidatorHash mintDatum claimPrice
    servicePubKey <- pkhFromHexStringM (mctdrpServicePkh params)
    let servicePayConstraint = Constraints.mustPayToPubKey  (PaymentPubKeyHash servicePubKey) (mctdrpServicePrice params)
    let ownerPayConstraint = Constraints.mustPayToPubKey (PaymentPubKeyHash (owner contractParams)) (publishPrice contractParams)
    utxoTxId  <- txIdFromHexStringM (mctdrpSubmitTxId  params)
    let utxo = TxOutRef {
        txOutRefId = utxoTxId,
        txOutRefIdx = mctdrpSubmitTxIdx params
    }
    let useConstraint = Constraints.mustSpendScriptOutput utxo emptyRedeemer
    let constraints = mintConstraint <> claimPayConstraint <> servicePayConstraint <> ownerPayConstraint <> useConstraint
    let script =claimCredTokenInstance onchainMintParams
    tx <- (submitTxConstraints script constraints)
    output <- case findOutputToValidatorHash claimValidatorHash tx of
                 Just output -> return output
                 Nothing -> throwError (GTextError "Can't find output to claim address in generated transaction")
    return $ MintCredTokenDATRequestResult {
        mctdrrTxId = show (txOutRefId output),
        mctdrrTxIdx = txOutRefIdx output
    }

emptyRedeemer :: Redeemer
emptyRedeemer = Redeemer (PlutusTx.toBuiltinData ())


claimDATCredToken :: Promise () CredTokenUserEndpoints GError ()
claimDATCredToken = endpoint @"claimDATCredToken" @ClaimCredTokenDATParams $ \params -> do
    let binCredentialRequest = stringToBuiltinByteString (cctpCredentialRequest params)
    let typedClaimDatum = CredentialMintClaimDatum {
        cmcdCredentialRequest = binCredentialRequest,
        cmcdType = CredTokenDAT,
        cmcdCode = stringToBuiltinByteString (cctpCode params) 
    }
    let claimDatum = Datum (PlutusTx.toBuiltinData typedClaimDatum)
    inTxId <- txIdFromHexStringM (cctpTxId params) 
    let utxoRef = TxOutRef {
        txOutRefId = inTxId,
        txOutRefIdx = (cctpTxIdx params)
    }
    let useConstraint = Constraints.mustSpendScriptOutput utxoRef emptyRedeemer
    ppkh <- ownPaymentPubKeyHash
    mbInTxOut <- unspentTxOutFromRef utxoRef
    inTxOut <- case mbInTxOut of
                   Nothing -> throwError (GTextError  "Can't find txOut in chain index")
                   Just txOut -> return txOut
    let payConstraint = Constraints.mustPayWithDatumToPubKey ppkh claimDatum (_ciTxOutValue inTxOut)
    -- Plutus specific --- we should have ScriptLookups filled to have transaction.
    -- Actually it's strange -- why we can't get something from script-address without 
    -- knowing scripts.
    let lookups :: ScriptLookups ClaimCredToken  = unspentOutputs (Map.singleton utxoRef inTxOut)
    tx <- submitTxConstraintsWith lookups payConstraint
    return ()


computeCredRequestCurrencySymbol :: CredTokenParams -> CurrencySymbol
computeCredRequestCurrencySymbol params =
    let daMintingPolicyHash = UtilsScripts.mintingPolicyHash (credTokenDATMintingPolicyScript params)
    in 
        Value.mpsSymbol daMintingPolicyHash

credTypeFromStringM :: forall w s. String -> Contract w s GError CredTokenType
credTypeFromStringM s =
    case s of
        "DAT" -> return CredTokenDAT
        "NFT" -> return CredTokenNFT
        "NTT" -> return CredTokenNTT
        _     -> throwError (GTextError "Invalid cred-token type")

getCredRequestCurrencySymbol :: Promise () CredTokenServiceEndpoints GError String
getCredRequestCurrencySymbol = endpoint @"getCredRequestCurrencySymbol" @GetCredRequestCurrencySymbolParams $ \params -> do
    logInfo @String $ "request currency symbolf for cred-request "
    let binCredRequest = stringToBuiltinByteString (gcrcspCredRequest params)
    authority <- pkhFromHexStringM (gcrcspAuthorityPkh params)
    credType <- credTypeFromStringM (gcrcspCredType params)
    let credTokenParams = CredTokenParams {
        ctpAuthority = authority,
        ctpCredentialRequest = binCredRequest,
        ctpType = credType
    }
    let currencySymbol = computeCredRequestCurrencySymbol credTokenParams
    return (show currencySymbol)


---
--- when user want to initiat NTT Token
---
submitNTTCodeRequestForDid :: ContractParams -> Promise () CredTokenUserEndpoints GError TxOutResult
submitNTTCodeRequestForDid contractParams = endpoint @"submitNTTCodeRequestForDid" @SubmitNTTCodeRequestForDidParams $ \params -> do
    logInfo @String $ "submit request for ntt"
    let binDid =  stringToBuiltinByteString ( sncrfdDid params )
    let binNonce = stringToBuiltinByteString (sncrfNonce params )
    let datum = Datum (PlutusTx.toBuiltinData (appendByteString binDid binNonce))
    let amount = (publishPrice contractParams) <> (scnrfServicePrice params)
    -- TODO:  script instead nttContractOwner ?
    let nttContractOwnerPkh = (nttPkh contractParams)
    let payConstraint = Constraints.mustPayWithDatumToPubKey (PaymentPubKeyHash nttContractOwnerPkh) datum amount
    tx <- submitTx payConstraint
    output <- findOutputToPubKeyHashM nttContractOwnerPkh tx
    return $ TxOutResult {
       torId = show (txOutRefId output),
       torIdx = txOutRefIdx output
    }




---
--- when authority initiate NTTToken for address,
--- assuming that authority know correct address and already recieve
--- payment in some way.
--- SubmitAuthorityNTTRequest :: ContractParams
mintNTTCredToken :: ContractParams -> Promise () CredTokenServiceEndpoints GError TxOutResult
mintNTTCredToken contractParams = endpoint @"mintNTTCredToken" @MintCredTokenNTTParams $ \params -> do
    logInfo @String $ "submit request for minting ntt"    
    let binCredentialRequest = stringToBuiltinByteString (mctnpCredentialRequest params)
    authority <- pkhFromHexStringM (mctnpServicePkh params) 
    let onchainMintParams = CredTokenParams {
        ctpAuthority = authority,
        ctpCredentialRequest = binCredentialRequest,
        ctpType = CredTokenNTT
    }
    let nttMintingPolicy = (credTokenNTTMintingPolicyScript onchainMintParams)
    let nttMintingPolicyHash = UtilsScripts.mintingPolicyHash nttMintingPolicy
    submitTxId <- txIdFromHexStringM (mctnpSubmitTxId params)
    let submitOutRef = TxOutRef {
        txOutRefId = submitTxId,
        txOutRefIdx = (mctnpSubmitTxIdx params)
    } 
    mbSubmitOut <- unspentTxOutFromRef submitOutRef
    submitOut <- case mbSubmitOut of
                     Nothing -> throwError (GTextError "Can't retrievesupplied transaction")
                     Just value -> return value
    let holderAddress = (view ciTxOutAddress submitOut)
    holderPkh <- case toPubKeyHash holderAddress of
                     Nothing -> throwError (GTextError "NTT shoukd not be minted for script address")
                     Just pkh -> return pkh
    let holderPkhBytes = Ledger.getPubKeyHash holderPkh
    let nttTokenName = TokenName holderPkhBytes
    let nttAmount = mctnpMintAmount params                 
    let mintConstraint = Constraints.mustMintCurrency nttMintingPolicyHash nttTokenName nttAmount
    let nttCurrencySymbol = Value.mpsSymbol nttMintingPolicyHash
    let nttValue = Value.singleton nttCurrencySymbol nttTokenName nttAmount
    let holderPayConstraints = Constraints.mustPayToPubKey (PaymentPubKeyHash holderPkh) nttValue
    let ownerPkh = owner contractParams
    let ownerPayConstraint = Constraints.mustPayToPubKey (PaymentPubKeyHash ownerPkh) (publishPrice contractParams)
    let servicePayConstraint = Constraints.mustPayToPubKey (PaymentPubKeyHash authority) (mctnpServicePrice params)
    let useConstraint = Constraints.mustSpendPubKeyOutput submitOutRef
    let binDid = stringToBuiltinByteString (mctnpDid params)
    let binNonce = stringToBuiltinByteString (mctnpNonce params)
    let datum = (Datum (PlutusTx.toBuiltinData (appendByteString binDid binNonce)))
    let datumConstraint = Constraints.mustIncludeDatum datum
    let constraints = mintConstraint <> holderPayConstraints <> ownerPayConstraint 
                      <> servicePayConstraint <> useConstraint <> datumConstraint
    let lookups  = generalise (mintingPolicy nttMintingPolicy)
    tx <- submitTxConstraintsWith lookups constraints
    output <- findOutputToPubKeyHashM holderPkh tx
    return $ TxOutResult {
        torId = show (txOutRefId output),
        torIdx =txOutRefIdx output
    }

