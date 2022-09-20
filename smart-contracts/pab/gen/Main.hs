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

import           Cardano.Api
import           Cardano.Api.Shelley                 (PlutusScript (..))
import           Codec.Serialise                     (serialise)
import           Data.Aeson                          (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX               (getPOSIXTime)
import           PlutusTx
import qualified PlutusTx
import qualified Ledger
import qualified Ledger.Scripts                       as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import qualified System.Environment

--import qualified Ledger.Typed.Scripts as Scripts
--import qualified PlutusLedgerApi.V1.Scripts as Scripts
import           ProofSpace.Contracts.OnChain.LockValue  


help :: IO (Either String String)
help = do
            _ <- print("Usage: proofspace-token-credential-gen script params")
            _ <- print("where script is one-of")
            _ <- print("   lock-value  [posix-time]")
            _ <- print("                           ")
            return (Left "No operation to perform" ) 

writeValidator :: FilePath -> Scripts.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Scripts.unValidatorScript

writeLockValueValidaror :: Ledger.POSIXTime -> FilePath -> IO (Either (FileError ()) ())
writeLockValueValidaror expireTime fname = do
    let scriptParams = LockValueParams {
        lvpExpireTime = expireTime
    }
    let validatorInstance = validateLockValueInstance scriptParams
    let script = Scripts.validatorScript validatorInstance
    writeResult <- writeValidator fname script
    return writeResult

genLockValue :: [String] -> IO (Either String String)
genLockValue nextArgs =
    if null nextArgs then
        return $ Left "lock-value require argument (epoch)"
    else 
        do
            let seconds = read (head nextArgs)::Integer
            -- this is epoch seconds.
            let expireTime = (fromInteger seconds)::NominalDiffTime
            now <- getPOSIXTime
            if (expireTime < now) then
                return  $ Left "Expire time is on the past"
            else
                do
                    let fname = "validator.dat"
                    writeResult <- writeLockValueValidaror (Ledger.POSIXTime seconds) "validator" 
                    let result = case writeResult of
                                    Left err  ->  Left (show err)
                                    Right _   ->  Right fname
                    return result
        

        
    



main :: IO ()
main = do
            args <- System.Environment.getArgs
            result <- if null args then 
                        help
                    else
                        case head args of
                            "lock-value" -> genLockValue (tail args)
                            other  ->
                                return (Left ("Unknown script: "<> other))            
            case result of
                Left message -> do
                                   _ <- print("No operation performed:" <> message)
                                   return ()
                Right fname  -> do 
                                   print("ok:"<>fname)
                                   return ()
                                   

        


