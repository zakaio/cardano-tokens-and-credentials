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

module Main(main) where

import           Data.Aeson                          (FromJSON (..), ToJSON (..))
import qualified Data.OpenApi                        as OpenApi
import           Data.Reflection                     (Given (..), give)
import           Data.Text                           (unpack)
import           GHC.Generics                        (Generic)
import           Plutus.V1.Ledger.Api                (Value, adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Value              as Value
import           Plutus.Contract                     (ContractError)
import           Plutus.PAB.Effects.Contract.Builtin (SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)
import           Prettyprinter                       (Pretty (..), viaShow)
    

import qualified Plutus.Contracts.OffChain.DidAddress as DidAddress
import           Plutus.Contracts.OffChain.DidAddress  (ContractParams (..))
import           Plutus.Contracts.OffChain.ProofspaceCommon (GError (..), pkhFromString) 


import           qualified PabContracts              as PabContracts
import           PabContracts                        (PabContracts (..))


contractParams :: String -> ContractParams 
contractParams pkhs = ContractParams {
       owner = case pkhFromString pkhs of 
                  Left(msg) -> error(unpack msg)
                  Right(pkh) -> pkh,
       publishPrice = Value.singleton adaSymbol  adaToken 2000000,
       backPrice = Value.singleton adaSymbol adaToken 2000000,
       daTokenAmount = 9999
}


main :: IO ()
main = do
    let pkhs = "TODO: read from file"
    (give (contractParams pkhs) 
          (runWith (handlers @PabContracts)))



-- here we can intercept handlers
handlers :: Builtin.HasDefinitions a => Builtin.BuiltinHandler a
handlers = Builtin.handleBuiltin

