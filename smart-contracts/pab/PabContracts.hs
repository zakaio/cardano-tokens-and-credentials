{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module PabContracts(PabContracts) where

import           Data.Aeson                          (FromJSON (..), ToJSON (..))
import qualified Data.OpenApi                        as OpenApi
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (ContractError)
import           Plutus.PAB.Effects.Contract.Builtin (SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.Contracts.OffChain.DidAddress         as DidAddress
import           Prettyprinter                       (Pretty (..), viaShow)


data PabContracts =
    DidAddressContract
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (OpenApi.ToSchema, ToJSON, FromJSON)

instance Pretty PabContracts where
    pretty = viaShow

instance Builtin.HasDefinitions PabContracts where
    getDefinitions = [DidAddressContract]
    getSchema =  \case
        DidAddressContract -> Builtin.endpointsToSchemas @DidAddress.DidAddressSchema
    getContract = \case
        DidAddressContract -> SomeBuiltin (DidAddress.didAddress @ContractError)


