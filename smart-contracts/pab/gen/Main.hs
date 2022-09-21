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
import           Data.Aeson.Encode.Pretty            (defConfig, encodePretty' )
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Time
import           Data.Time.Clock.POSIX               (getPOSIXTime)
import qualified Ledger
import qualified Ledger.Scripts                       as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import qualified System.Environment
import qualified System.Exit

import           ProofSpace.Contracts.OnChain.LockValue  

data Options = Options {
    outFile :: String,
    stdout :: Bool
}



help :: IO (Either String a)
help = do
            _ <- print("Usage: proofspace-token-credential-gen [options] script params")
            _ <- print("options is one of:")
            _ <- print("   --out fname     output script to fname (default: onchain.json)")
            _ <- print("   --stdout        output script to stdout")
            _ <- print("")
            _ <- print("and script params is one-of:")
            _ <- print("   lock-value  [posix-time]")
            _ <- print("                           ")
            return (Left "No operation to perform" ) 

writeValidator :: Options -> Scripts.Validator -> IO (Either (FileError ()) (Maybe String))
writeValidator options validator = do
    let serializedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Scripts.unValidatorScript
    if (stdout options) then do
        -- not exported form cardano-node
        --let content = textEnvelopeToJSON @(PlutusScript PlutusScriptV1) Nothing (serializedScript validator)
        let content = encodePretty'  defConfig (serialiseToTextEnvelope @(PlutusScript PlutusScriptV1)  Nothing (serializedScript validator))
        _ <- LBS.putStrLn(content)
        return (Right Nothing) 
    else
        do
            let file = outFile options
            writeResult <- writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing (serializedScript validator)
            let result = case writeResult of
                    Left err -> Left err
                    Right _  -> Right (Just file) 
            return result
        


writeLockValueValidaror :: Ledger.POSIXTime -> Options -> IO (Either (FileError ()) (Maybe String))
writeLockValueValidaror expireTime options = do
    let scriptParams = LockValueParams {
        lvpExpireTime = expireTime
    }
    let validatorInstance = validateLockValueInstance scriptParams
    let script = Scripts.validatorScript validatorInstance
    writeResult <- writeValidator options script
    return writeResult

genLockValue :: [String] -> Options -> IO (Either String (Maybe String))
genLockValue nextArgs options =
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
                    writeResult <- writeLockValueValidaror (Ledger.POSIXTime seconds) options
                    let result = case writeResult of
                                    Left err  ->  Left (show err)
                                    Right optFname   ->  Right optFname
                    return result

              

command :: [String] -> Options  -> IO (Either String (Maybe String))
command args options =  
            if null args then 
                help
            else
                case head args of
                    "--out"  -> continueWithOut
                    "++out"  -> continueWithOut
                    "--stdout" -> continueWithStdout
                    "++stdout"  -> continueWithStdout
                    "lock-value" -> genLockValue (tail args) options
                    other  ->
                            return (Left ("Unknown script: "<> other))
                where
                    continueWithOut = commandWithOutOption (tail args) options
                    continueWithStdout = command (tail args) (options {stdout = True})



commandWithOutOption :: [String] -> Options -> IO (Either String (Maybe String))
commandWithOutOption args options = 
    if null args then
        return (Left "--out option miss fname")
    else
        command (tail args) (options { outFile = (head args) })
        


main :: IO ()
main = do
            args <- System.Environment.getArgs
            let emptyOptions = Options {
                outFile = "onchain.json",
                stdout = False
            }
            result <- command args emptyOptions
            case result of
                Left message -> do
                                   _ <- System.Exit.die("No operation performed:" <> message)
                                   return ()
                Right (Just fname)  -> do 
                                     print("ok:"<> fname)
                                     return ()
                Right Nothing -> return ()
                                   

        


