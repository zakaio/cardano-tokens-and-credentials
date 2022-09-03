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
    txIdFromHexString,
    findOutputToPubKeyHash
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
                                       TxOutRef (..), 
                                       CardanoTx,
                                       getCardanoTxOutRefs,
                                       toPubKeyHash) 
import           Ledger.Bytes         (LedgerBytes(LedgerBytes), fromHex)
import           Plutus.V1.Ledger.Api (BuiltinByteString (..))


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
   

txIdFromHexString :: String -> Either Text TxId   
txIdFromHexString s = 
    fmap TxId (byteStringFromHexString s)      
   

byteStringFromHexString :: String -> Either Text BuiltinByteString
byteStringFromHexString s =         
    case fromHex (BS.pack s) of 
        Right (LedgerBytes bytes) -> Right $ bytes 
        Left msg -> Left $ pack ("Could not convert from hex to bytes: " <> msg)


--
-- fir online
--
findOutputToPubKeyHash :: PubKeyHash -> CardanoTx -> Maybe TxOutRef
findOutputToPubKeyHash pkh tx =
    let txOutputs = getCardanoTxOutRefs tx 
        myTxOutputs = filter (\x ->
                        case toPubKeyHash (txOutAddress (fst x)) of
                            Nothing -> False
                            Just txPkh -> pkh == txPkh
                              )  txOutputs
    in
        case myTxOutputs  of
            [] -> Nothing
            (x:rest) -> Just (snd x)    
