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

import           Data.Aeson                          (FromJSON (..), ToJSON (..))
import           ProofSpace.Contracts.OnChain.LockValue  


main :: IO ()
main = do
           _ <- print("main")
           return ()


