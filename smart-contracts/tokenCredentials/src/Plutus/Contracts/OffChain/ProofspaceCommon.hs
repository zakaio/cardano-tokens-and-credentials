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
    pkhFromString
) where


import           Prelude
import           GHC.Generics
import           Control.Lens
import           Data.Aeson           (FromJSON (..), ToJSON (..))
import           Data.Either
import           Data.Text
import qualified Data.ByteString.Char8 as BS
import           Plutus.Contract
import           Ledger               (PubKeyHash(..), pubKeyHash) 
import           Ledger.Bytes         (LedgerBytes(LedgerBytes), fromHex)


data GError =   GTextError Text
                    |
                GContractError ContractError
                deriving stock (Show, Eq, Generic)
                deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''GError

instance AsContractError GError where
    _ContractError = _GContractError


pkhFromString :: String -> Either Text PubKeyHash   
pkhFromString s =         
    case fromHex (BS.pack s) of 
        Right (LedgerBytes bytes) -> Right $ PubKeyHash bytes 
        Left msg -> Left $ pack ("Could not convert from hex to bytes: " <> msg)