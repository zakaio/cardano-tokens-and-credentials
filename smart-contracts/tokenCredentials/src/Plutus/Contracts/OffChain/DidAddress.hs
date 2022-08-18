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



-- | Receive 
module Plutus.Contracts.OffChain.DidAddress
    ( submitDid
    , claimDid
    , mintDid
    -- TODO: finalizePublish
    , DidAddressSchema (..)
    , SubmitDidParams (..)
    , ClaimDidParams (..)
    -- Patams
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
import           Data.Maybe            (catMaybes, listToMaybe)
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
import           Ledger                (PaymentPubKeyHash (..), unPaymentPubKeyHash, CurrencySymbol)
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Constraints.OffChain (mintingPolicy)
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
import           Plutus.Contracts.OnChain.DidAddress (SubmitDidDatum (..),
                                                      ClaimDidDatum (..),
                                                      didAddressMintingPolicyScript,
                                                      didAddressTokenCurrency,
                                                      submitDidInstance,
                                                      submitDidValidator,
                                                      claimDidInstance,
                                                      claimDidValidator
                                                     )
import           Plutus.Contracts.OffChain.ProofspaceCommon (GError (..), pkhFromString) 



data ContractParams = ContractParams {
        owner :: PubKeyHash,
        publishPrice :: Value,
        backPrice :: Value,
        daTokenAmount :: Integer
     }

type DidAddressUserEndpoints =
         Endpoint "submitDid" SubmitDidParams
     .\/ Endpoint "claimDid"  ClaimDidParams

type DidAddressOwnerEndpoints =
         Endpoint "mintDid" MintDidParams

type DidAddressSchema =
           DidAddressUserEndpoints
      .\/  DidAddressOwnerEndpoints

-- submit did is done by requester
newtype SubmitDidParams = SubmitDidParams {
       sdpDid:: String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data MintDidParams = MintDidParams {
       mdpDid:: String,
       mdpSPkh:: String,
       mdpCode:: String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


data ClaimDidParams = ClaimDidParams
    { 
      cdpDid :: String,
      cdpCode :: String 
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)



-- | The "submitDid" contract, 
-- |  Th work is started by did holder, who wnat to link his did with address.
submitDid :: AsContractError e => ContractParams -> Promise () DidAddressUserEndpoints e ()
submitDid contractParams = endpoint @"submitDid" @SubmitDidParams $ \(SubmitDidParams did) -> do
    logInfo @String $ "Submit " <> show did <> " to the script"
    pkh <- (fmap unPaymentPubKeyHash ownPaymentPubKeyHash)
    let didDatum  = convertSubmitDidDatum did pkh
    let tx = Constraints.mustPayToTheScript didDatum (publishPrice contractParams)
    void (submitTxConstraints (submitDidInstance (owner contractParams)) tx)


convertSubmitDidDatum :: String -> PubKeyHash -> SubmitDidDatum
convertSubmitDidDatum did pkh = (SubmitDidDatum (stringToBuiltinByteString did) pkh)




-- | mint Did 
-- |  (here we should tell the queur of address and submitted did to external system)
-- |  (currently we do not model this here. In general, this work is actually handled
-- |   by external system and copy here is only for contract modelling).
mintDid :: ContractParams -> Promise () DidAddressOwnerEndpoints GError ()
mintDid contractParams = endpoint @"mintDid" @MintDidParams $ \(MintDidParams did spkh code) -> do
    let bDid = (stringToBuiltinByteString did)
    pkh <- case (pkhFromString spkh) of
                 Left(message) -> throwError (GTextError message)
                 Right(value) -> return value
    let submitDidDatumRecord = SubmitDidDatum {
        dtSubmitDid = bDid,
        dtSubmitPkh = pkh
    }
    let submitDidDatumData = PlutusTx.toBuiltinData submitDidDatumRecord
    let submitDidDatum = Datum submitDidDatumData
    let contractOwner = (owner contractParams)
    let submitDidAddr =  Ledger.scriptAddress (submitDidValidator contractOwner) 
    let overallPrice = (publishPrice contractParams) PlutusTxNumeric.+ (backPrice contractParams)
    -- toMeUtxos <- fundsAtAddressGeq submitDidAddr overallPrice
    toMeUtxo <-  utxosAt submitDidAddr
    let myUtxo = Map.filter (\utxo -> isUtxoWithDatum utxo submitDidDatum) (toMeUtxo) 
    myTxOutRef  <- if (Map.null myUtxo) then
                    throwError (GTextError "utxo with datum is not found")
                 else
                    return (fst (List.head (Map.toList myUtxo)))
    let myChainIndexTxOut = myUtxo ! myTxOutRef 
    let utxoValue :: Ledger.Value = view ciTxOutValue  myChainIndexTxOut
    constraints <-  if  not (( overallPrice /\ utxoValue) == overallPrice) then
                        --TODO: enable after debug
                        --logInfo "payment is less then neede" >>
                        --return (Constraints.mustPayToPubKey contractOwner (txOutValue txOut))
                        throwError (GTextError "payment is less then needed")
                    else
                         do 
                            _ <- logInfo @String "sending payments"
                            let claimDidValidatorHash = (UtilsScripts.validatorHash (claimDidValidator contractOwner))
                            let claimDatumRecord = ClaimDidDatum {
                                dtClaimDid = bDid,
                                dtClaimPkh = pkh,
                                dtClaimCode = (stringToBuiltinByteString code)
                            }
                            let claimDidDatum = Datum (PlutusTx.toBuiltinData(claimDatumRecord))
                            let didAddressAmount = daTokenAmount contractParams
                            let didAddressValue = createDAValue contractOwner pkh bDid didAddressAmount
                            let claimPrice = didAddressValue PlutusTxNumeric.+ (backPrice contractParams)
                            let daMintingPolicyHash = (UtilsScripts.mintingPolicyHash (didAddressMintingPolicyScript contractOwner pkh bDid) )
                            let constraints = (
                                   (Constraints.mustSpendScriptOutput  myTxOutRef voidRedeemer) <>
                                   (Constraints.mustPayToPubKey (PaymentPubKeyHash contractOwner) (publishPrice contractParams)) <>
                                   (Constraints.mustMintCurrency daMintingPolicyHash daTokenName didAddressAmount) <>
                                   (Constraints.mustPayToOtherScript claimDidValidatorHash claimDidDatum claimPrice)
                                 )
                            return constraints
    void   (submitTxConstraints (claimDidInstance contractOwner) constraints)
    
    
isUtxoWithDatum :: ChainIndexTxOut -> Datum -> Bool
isUtxoWithDatum chTxOut datum =
    let datumOrHash :: (Either DatumHash Datum) = _ciTxOutDatum chTxOut
    in
     case datumOrHash of
        Left hash -> 
            let datumHash =  UtilsScripts.datumHash datum
            in datumHash == hash
        Right dt  -> datum == dt
    


daTokenName :: TokenName
daTokenName = TokenName "did:prism"

voidRedeemer :: Redeemer
voidRedeemer = Redeemer (PlutusTx.toBuiltinData ())

createDAValue :: PubKeyHash -> PubKeyHash -> BuiltinByteString -> Integer -> Value
createDAValue  contractOwner didOwner bDid amount =
    let 
        didAddresCs :: CurrencySymbol = (didAddressTokenCurrency contractOwner didOwner bDid)
        didAddressTkn :: TokenName = daTokenName
        didAddressAmount = amount
    in
        Value.singleton didAddresCs didAddressTkn didAddressAmount


-- | The "claimDid" contract endpoint. See note [Contract endpoints]
-- | This code is called, when dit owner receive the code and want 
-- | to receive 
claimDid :: ContractParams -> Promise () DidAddressUserEndpoints GError ()
claimDid contractParams = endpoint @"claimDid" @ClaimDidParams $ \(ClaimDidParams did code) -> do
    -- In a real use-case, we would not submit the transaction if we know that code is invalie
    -- wrong.
    logInfo @String "Submitting transaction to claim did"
    let claimAddress = Ledger.scriptAddress (claimDidValidator (owner contractParams))
    ppkh <- ownPaymentPubKeyHash
    let pkh = unPaymentPubKeyHash ppkh
    let bDid = (stringToBuiltinByteString did)
    let claimDatumRecord = ClaimDidDatum {
            dtClaimDid = bDid,
            dtClaimPkh = pkh,
            dtClaimCode = (stringToBuiltinByteString code)
    }
    let claimDidDatum = Datum (PlutusTx.toBuiltinData(claimDatumRecord))
    let didAddressValue = createDAValue (owner contractParams) pkh bDid (daTokenAmount contractParams)
    let claimValue = didAddressValue PlutusTxNumeric.+ (backPrice contractParams)
    utxos <-  utxosAt claimAddress 
    let myUtxos = Map.filter (\utxo -> isUtxoWithDatum utxo claimDidDatum) utxos 
    _ <- if (Map.null myUtxos) then
            throwError (GTextError  "Can't fibd ckaim did datum in input")
         else 
            return () 
    let constraints = Constraints.mustPayWithDatumToPubKey ppkh claimDidDatum claimValue
    let claimScriptInstance = claimDidInstance (owner contractParams)
    void (submitTxConstraintsSpending claimScriptInstance myUtxos constraints)



didAddressUserContract :: ContractParams -> Contract () DidAddressUserEndpoints GError ()
didAddressUserContract params = do
    logInfo @String "Waiting for guess or lock endpoint..."
    selectList [(submitDid params), (claimDid params)] >> (didAddressUserContract params)

didAddressOwnerContract :: ContractParams -> Contract () DidAddressOwnerEndpoints GError ()
didAddressOwnerContract params = do
    logInfo @String "Waiting for contract "
    selectList [mintDid params] >> (didAddressOwnerContract params)
 

