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
      submitCredTokenCodeDATRequest
    --, mintCredToken,
    --, claimCredToken,
    --, getCredRequestCurrencySymbol
    --, submitNTTCodeRequestForDid
    --, claimNTTCode
    --, mintNTTCredToken
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
import           Data.Map              (Map, (!))
import qualified Data.Map              as Map
import qualified Data.List             as List
import           Data.Maybe          
import           Data.Text             (Text (..), pack)
import           Plutus.V1.Ledger.Api  (Address, ScriptContext, Validator, Value, 
                                        Datum(Datum), DatumHash(DatumHash), PubKeyHash,
                                        Redeemer (Redeemer),
                                        BuiltinByteString,
                                        getMintingPolicy
                                        )
import qualified Plutus.V1.Ledger.Address as Address
import qualified Plutus.V1.Ledger.Value as Value
import qualified Ledger
import           Ledger                (PaymentPubKeyHash (..), unPaymentPubKeyHash, 
                                       CurrencySymbol,
                                       txOutRefId, txOutRefIdx)
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Constraints.OffChain (ScriptLookups,mintingPolicy,otherScript)
import           Ledger.Tx             (ChainIndexTxOut (..), ciTxOutDatum, ciTxOutValue)
import           Playground.Contract
import           Plutus.Contract
import           Plutus.Contract       (ContractError (..))
import qualified PlutusTx
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import           PlutusTx.Builtins       (sha2_256)
import           PlutusTx.Lattice        ( (/\) )
import qualified PlutusTx.Numeric       as PlutusTxNumeric
import           Prelude             
import qualified Plutus.Script.Utils.V1.Scripts as UtilsScripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as TScripts

import           Plutus.Contracts.OffChain.ProofspaceCommon (GError (..), 
                                                             pkhFromHexString,
                                                             findOutputToPubKeyHash) 
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
                                    claimCredTokenValidator
                                             )


data ContractParams = ContractParams {
        owner :: PubKeyHash,
        publishPrice :: Value,
        claimBackPrice :: Value,
        credentialAmount :: Integer
     }
     deriving stock (Eq, Show, Generic)
     deriving anyclass (FromJSON, ToJSON)


type CredTokenUserEndpoints =
         Endpoint "submitCredTokenDATRequest" SubmitCredTokenDATRequestParams
     .\/ Endpoint "claimDATCredToken"  ClaimCredTokenDATParams


type CredTokenServiceEndpoints =
         Endpoint "mintDATCredToken"  MintCredTokenDATRequestParams


data SubmitCredTokenDATRequestParams = SubmitCredTokenDATRequestParams {        
        sctdrpCredentialRequest:: !String,
        sctdrpTokenName        :: !String,
        sctdrpDid              :: !String,
        sctdrpServicePkh       :: !String,
        sctdrpAmount           :: !Value
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data SumitCredTokenDATDatum = SubmitCredTokenDATDatum {
    sctddCredentialRequest:: !BuiltinByteString,
    sctddTokenName        :: !TokenName,
    sctddDid              :: !BuiltinByteString
}

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
        mctdrpAmount           :: !Value,
        mctdrpSubmitTxId       :: !String
}

type MintCredTokenDATRequestResult = ()

data ClaimCredTokenDATParams = ClaimCredTokenDATParams {
        cctpCredentialRequest :: !String,
        cctpTokenName :: !String,
        cctpCode :: !String,
        cctpTxId:: !String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data MintCredTokenParams = MintCredTokenParams {
    mctpTxId :: !String,
    mctpTxInd :: !Integer,
    mctpCredentialRequest :: !BuiltinByteString
}

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
    


mintDATCredToken :: AsContractError e => Promise () CredTokenServiceEndpoints e MintCredTokenDATRequestResult 
mintDATCredToken =  endpoint @"mintDATCredToken" @MintCredTokenDATRequestParams $ \params -> do
    let binCredentialRequest = stringToBuiltinByteString (mctdrpCredentialRequest params)
    authority <- case pkhFromHexString (mctdrpServicePkh params) of
                        Left msg -> throwError (GTextError msg)
                        Right value -> return value 
    let onchainMintParams = CredTokenParams {
        ctpAuthority = authority,
        ctpCredentialRequest = binCredentialRequest,
        ctpType = CredTokenDAT
    }
    let datMintingPolicy = mintingPolicy (credTokenDATMintingPolicyScript onchainMintParams)
    let datMintingPolicyLookup =
    let binCredentialRequest = stringToBuiltinByteString (mctdrpCredentialRequest params)
    let tokenName = mctdrpTokenName params
    let mintAmount = mctdrpAmount params
    let mintConstraint = Constraints.mustMintCurrency datMintingPolicy tokenName
    let price = (publishPrice params) <> (claimBackPrice params)
    let typedMintDatum = CredentialMintClaimDatum {
        cmcdCredentialRequest = binCredentialRequest,
        cmcdType = CredTokenDAT,
        cmcdCode = mctdrpCode params
    }
    let mintDatum = Datum $ PlutusTx.toBuiltinData typedMintDatum
    let payConstraint = Constraints.mustPayWithDatumToPubKey mintDatum price
    let constraints = mintConstraint <> payConstraint
    let script = ClaimCredTokenInstance
    void (submitTxConstraints script constraints)
