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


module Plutus.Contracts.OffChain.ProofspaceCommon(
    GError (..),
    pkhFromHexString,
    pkhFromHexStringM,
    txIdFromHexString,
    txIdFromHexStringM,
    findOutputToPubKeyHash,
    findOutputToPubKeyHashM,
    findOutputToValidatorHash,
    tokenNameFromHexString,
    tokenNameFromHexStringM,
    byteStringFromHexString,
    byteStringFromHexStringM
) where


import           Prelude
import           GHC.Generics
import           Control.Lens
import           Data.Aeson           (FromJSON (..), ToJSON (..))
import           Data.Either
import           Data.Text            (Text (..), pack)
import qualified Data.ByteString.Char8 as BS
import           Plutus.Contract
import           Ledger               (PubKeyHash(..), pubKeyHash, 
                                       TxId (..), 
                                       txOutAddress,
                                       TxOut,
                                       TxOutRef (..), 
                                       CardanoTx,
                                       getCardanoTxOutRefs,
                                       toPubKeyHash,
                                       toValidatorHash
                                       ) 
import           Ledger.Bytes         (LedgerBytes(LedgerBytes), fromHex)
import           Plutus.V1.Ledger.Api (BuiltinByteString (..), TokenName (..), ValidatorHash (..))


data GError =   GTextError Text
                    |
                GContractError ContractError
                deriving stock (Show, Eq, Generic)
                deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''GError

instance AsContractError GError where
    _ContractError = _GContractError



pkhFromHexString :: String -> Either Text PubKeyHash   
pkhFromHexString s =     
    fmap PubKeyHash (byteStringFromHexString s)    
      
pkhFromHexStringM :: forall w s. String -> Contract w s GError PubKeyHash
pkhFromHexStringM s = do
    bytes <- byteStringFromHexStringM s
    return (PubKeyHash bytes)      

txIdFromHexString :: String -> Either Text TxId   
txIdFromHexString s = 
    fmap TxId (byteStringFromHexString s)      

txIdFromHexStringM :: forall w s. String -> Contract w s GError TxId
txIdFromHexStringM s = do
    bytes <- byteStringFromHexStringM s
    return (TxId bytes)
   
tokenNameFromHexString :: String -> Either Text TokenName
tokenNameFromHexString s =
    fmap TokenName (byteStringFromHexString s)

tokenNameFromHexStringM :: forall w s. String -> Contract w s GError TokenName
tokenNameFromHexStringM s = do
     bytes <- byteStringFromHexStringM s
     return (TokenName bytes)


byteStringFromHexString :: String -> Either Text BuiltinByteString
byteStringFromHexString s =         
    case fromHex (BS.pack s) of 
        Right (LedgerBytes bytes) -> Right $ bytes 
        Left msg -> Left $ pack ("Could not convert from hex to bytes: " <> msg)

byteStringFromHexStringM :: forall w s. String -> Contract w s GError BuiltinByteString
byteStringFromHexStringM s =
    case byteStringFromHexString s of
        Left  msg -> throwError (GTextError msg)
        Right bytes -> return bytes
    


--
-- fir online
--
findCardanoTxOut :: (TxOut -> Bool) -> CardanoTx -> Maybe TxOutRef
findCardanoTxOut p tx =
    let txOutputs = getCardanoTxOutRefs tx 
        myTxOutputs = filter (\x -> p (fst x)) txOutputs
    in
        case myTxOutputs  of
            [] -> Nothing
            (x:rest) -> Just (snd x)    


findOutputToPubKeyHash :: PubKeyHash -> CardanoTx -> Maybe TxOutRef
findOutputToPubKeyHash pkh tx =
    findCardanoTxOut (\txOut ->
                        case toPubKeyHash (txOutAddress txOut) of
                            Nothing -> False
                            Just txPkh -> pkh == txPkh
                       )  tx

findOutputToPubKeyHashM :: forall w s. PubKeyHash -> CardanoTx -> Contract w s GError TxOutRef
findOutputToPubKeyHashM pkh tx =
    case findOutputToPubKeyHash pkh tx of
        Nothing     -> throwError (GTextError "Can't find output to given pkh")
        Just outRef -> return outRef


findOutputToValidatorHash :: ValidatorHash -> CardanoTx -> Maybe TxOutRef
findOutputToValidatorHash vh tx =
    findCardanoTxOut (\txOut ->
                         case toValidatorHash (txOutAddress txOut) of
                            Nothing -> False
                            Just txVh -> vh == txVh
                          ) tx
