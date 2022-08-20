{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}


module Main(main) where


import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Default                        (def)
import           Data.Aeson                          (FromJSON (..), ToJSON (..))
import qualified Data.Either                         as Either
import qualified Data.OpenApi                        as OpenApi
import           Data.Reflection                     (Given (..), give)
import           Data.Text                           (unpack)
import           GHC.Generics                        (Generic)
import           Plutus.V1.Ledger.Api                (PubKeyHash (..), Value, adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Value              as Value
import           Plutus.Contract                     (ContractError)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler(..), SomeBuiltin(..), SomeBuiltinState(..),
                                                     )
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers, Simulation(..))
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Wallet.Emulator.Wallet as Wallet
import           Prettyprinter                       (Pretty (..), viaShow)

--for handler
import           Playground.Types                    (FunctionSchema)
import           Schema                              (FormSchema)
import Plutus.PAB.Effects.Contract                   (ContractEffect (..))

-- local
import           Plutus.Contracts.OffChain.ProofspaceCommon (GError (..), pkhFromString) 
import           Plutus.Contracts.OffChain.DidAddress
--import qualified PabContracts                        as PabContracts
import           PabContracts                        (PabContracts (..))



main :: IO ()
main = void $ do
         -- TODO: implement read-file
         _ <- print("main")
         let ownerPkh = case pkhFromString "TODO" of
                           Left(msg) -> error (unpack msg)
                           Right(pkh) -> pkh
         let cfg = ContractParams {
            owner = ownerPkh,
            publishPrice = Value.singleton adaSymbol  adaToken 2000000,
            backPrice = Value.singleton adaSymbol  adaToken 2000000,
            daTokenAmount = 9999
         }
         let configuredHandlers = give cfg handlers               
         Simulator.runSimulationWith configuredHandlers simulation

simulation:: Simulation (Builtin PabContracts) ()
simulation =
    do
        Simulator.logString @(Builtin PabContracts) "Starting plutus-starter PAB webserver on port 9080. Press enter to exit."

        (wallet, _paymentPubKeyHash) <- Simulator.addWallet
        Simulator.waitNSlots 1
        liftIO $ writeFile "scripts/wallet" (show $ Wallet.getWalletId wallet)

        shutdown <- PAB.Server.startServerDebug

        -- Example of spinning up a game instance on startup
        -- void $ Simulator.activateContract (Wallet 1) GameContract
        -- You can add simulator actions here:
        -- Simulator.observableState
        -- etc.
        -- That way, the simulation gets to a predefined state and you don't have to
        -- use the HTTP API for setup.

        -- Pressing enter results in the balances being printed
        void $ liftIO getLine

        Simulator.logString @(Builtin PabContracts) "Balances at the end of the simulation"
        b <- Simulator.currentBalances
        Simulator.logBalances @(Builtin PabContracts) b

        shutdown



handlers :: Given ContractParams => SimulatorEffectHandlers (Builtin PabContracts)
handlers =
    Simulator.mkSimulatorHandlers def
    $ interpret (contractHandler Builtin.handleBuiltin)


