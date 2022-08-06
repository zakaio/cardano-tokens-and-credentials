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

import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin


import           PabContracts                        (PabContracts)
import           Plutus.PAB.Run                      (runWith)

main :: IO ()
main = do
    runWith (handlers @PabContracts)

-- here we can intercept handlers
handlers :: Builtin.HasDefinitions a => Builtin.BuiltinHandler a
handlers = Builtin.handleBuiltin

