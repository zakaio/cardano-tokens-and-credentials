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

-- for Given ContractParams => Builtin.HasDefinitions PabContracts
{-# LANGUAGE UndecidableInstances     #-}


module PabContracts(PabContracts (..)
                    ) where

import           Data.Aeson                          (FromJSON (..), ToJSON (..))
import qualified Data.OpenApi                        as OpenApi
import           Data.Reflection                     (Given (..), give)
import           GHC.Generics                        (Generic)
import           Plutus.V1.Ledger.Api                (Value, adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Value              as Value
import           Plutus.Contract                     (ContractError)
import           Plutus.PAB.Effects.Contract.Builtin (SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Prettyprinter                       (Pretty (..), viaShow)
import           Plutus.Contracts.OffChain.DidAddress       as DidAddress
import           Plutus.Contracts.OffChain.LockValue        as LockValue



data PabContracts = 
      DidAddressUserPabContract 
      |
      DidAddressOwnerPabContract 
      |
      LockValueContract
    deriving (Eq,Ord,  Show, Generic)
    deriving anyclass (OpenApi.ToSchema, ToJSON, FromJSON)

instance Pretty PabContracts  where
    pretty = viaShow


instance Given ContractParams => Builtin.HasDefinitions PabContracts where
      getDefinitions = [DidAddressUserPabContract, DidAddressOwnerPabContract, LockValueContract]
      getSchema = \case
          DidAddressUserPabContract  -> Builtin.endpointsToSchemas @DidAddress.DidAddressUserEndpoints
          DidAddressOwnerPabContract -> Builtin.endpointsToSchemas @DidAddress.DidAddressOwnerEndpoints
          LockValueContract          -> Builtin.endpointsToSchemas @LockValue.LockValueSchema
      getContract = \case
          DidAddressUserPabContract   -> SomeBuiltin (DidAddress.didAddressUserContract given)
          DidAddressOwnerPabContract  -> SomeBuiltin (DidAddress.didAddressOwnerContract given)
          LockValueContract           -> SomeBuiltin (LockValue.lockValueContract)


