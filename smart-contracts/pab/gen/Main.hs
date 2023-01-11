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
import           Cardano.Api.Shelley                 (PlutusScript (..), shelleyPayAddrToPlutusPubKHash)
import           Codec.Serialise                     (serialise)
import qualified Codec.Binary.Bech32   as Bech32
import           Data.Aeson.Encode.Pretty            (encodePretty )
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Text            as  Text
import           Data.Time
import           Data.Time.Clock.POSIX               (getPOSIXTime)
import qualified Ledger
import qualified Ledger.Scripts                       as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V1.Scripts       as UntypedScripts
import           PlutusTx.Builtins.Class              (stringToBuiltinByteString)
import qualified System.Environment
import qualified System.Exit

import           ProofSpace.Contracts.OnChain.LockValue  
import           ProofSpace.Contracts.OnChain.CredToken
import           ProofSpace.Contracts.OffChain.ProofspaceCommon (pkhFromHexStringM)

data Options = Options {
    outFile :: String,
    stdout :: Bool
}



help :: IO (Either String a)
help = do
            putStrLn("Usage: proofspace-token-credential-gen [options] script params")
            putStrLn("options is one of:")
            putStrLn("   --out fname     output script to fname (default: onchain.json)")
            putStrLn("   --stdout        output script to stdout")
            putStrLn("")
            putStrLn("and script params is one-of:")
            putStrLn("   lock-value  [posix-time]")
            putStrLn("   mint-ntt address credentialString")
            putStrLn("")
            return (Left "No operation to perform" ) 

writeValidator :: Options -> Scripts.Validator -> IO (Either (FileError ()) (Maybe String))
writeValidator options validator = do
    let serialiseScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Scripts.unValidatorScript
    let serialisedScript = serialiseScript validator
    let scriptAddress = Ledger.scriptAddress validator
    let scriptJson = encodePretty (serialiseToTextEnvelope @(PlutusScript PlutusScriptV1)  Nothing serialisedScript)
    let addressJson = encodePretty scriptAddress
    let content = "[\n"  <> (LChar8.unpack addressJson) <> "\n,\n" <> (LChar8.unpack scriptJson) <> "\n]"
    writeContent options content
        
writeContent :: Options -> String -> IO (Either (FileError ()) (Maybe String))
writeContent options content =
     if (stdout options) then do
        putStrLn(content)
        return (Right Nothing)
    else
        do
            let file = outFile options
            _ <- writeFile file content
            return (Right (Just file))


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


writeMintingPolicy :: Options -> Scripts.MintingPolicy -> IO (Either (FileError ()) (Maybe String) )
writeMintingPolicy  options mintingPolicy = do
     let serialiseScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Scripts.unMintingPolicyScript
     let serialisedScript = serialiseScript mintingPolicy
     let scriptHash = UntypedScripts.mintingPolicyHash mintingPolicy
     let scriptJson =  encodePretty (serialiseToTextEnvelope @(PlutusScript PlutusScriptV1)  Nothing serialisedScript)
     let scriptHashJson = encodePretty scriptHash
     let content = "[\n"  <> (LChar8.unpack scriptHashJson) <> "\n,\n" <> (LChar8.unpack scriptJson) <> "\n]"
     writeContent options content

writeCredNTTMintingScript :: String -> String -> Options -> IO (Either (FileError ()) (Maybe String) )
writeCredNTTMintingScript  address credentialString options = do
     let addr = case (deserialiseFromBech32 AsShelleyAddress (Text.pack address)) of
                   Right addr -> addr
                   Left decodingError -> error (show decodingError)
     let pkh = case shelleyPayAddrToPlutusPubKHash addr of
                   Just pkh -> pkh
                   Nothing  -> error ("given address (" ++ (show address) ++") is not based on public key")
     let binCredentialString = stringToBuiltinByteString credentialString
     let mintParams = CredTokenParams {
            ctpAuthority = pkh,
            ctpCredentialRequest = binCredentialString,
            ctpType = CredTokenNTT
     }
     let nttMintingPolicy = (credTokenNTTMintingPolicyScript mintParams)
     writeMintingPolicy options nttMintingPolicy

genMintNTT :: [String] -> Options -> IO (Either String (Maybe String))
genMintNTT nextArgs options =
    case nextArgs of
        [address,credentialString] -> do
            writeResult <- writeCredNTTMintingScript address credentialString options
            let result = case result of
                           Left err -> Left (show err)
                           Right optFname -> Right optFname
            return result
        _  ->
            return $ Left "mint-ntt require two arguments (address, credential-string), missing both"



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
                    "mint-ntt" -> genMintNTT (tail args) options
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
                                     putStrLn("ok:"<> fname)
                                     return ()
                Right Nothing -> return ()
                                   

        


