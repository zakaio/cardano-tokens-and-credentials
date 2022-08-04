{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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


-- | Receive 
module Plutus.Contracts.OffChain.DidAddress
    ( submitDid
    , claimDid
    , didAddress
    , DidAddressSchema
    , SubmitDidParams
    , ClaimDidParams
    ) where

import           Control.Monad         (void)
--import qualified Data.ByteString.Char8 as C
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Plutus.V1.Ledger.Api  (Address, ScriptContext, Validator, Value, Datum(Datum))
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Tx             (ChainIndexTxOut (..))
import           Playground.Contract
import           Plutus.Contract
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import qualified PlutusTx
--import           PlutusTx.Prelude      hiding (pure, (<$>))
--import qualified Prelude               as Haskell 
import           Prelude             
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import           Plutus.Contracts.OnChain.DidAddress 
import           Plutus.Contracts.OnChain.DidAddress (DidDatum (..))
--import qualified Network.Http.Client  as HttpClient


type DidAddressSchema =
        Endpoint "submitDid" SubmitDidParams
        .\/ Endpoint "claimDid" ClaimDidParams

-- | Parameters for the "submitDid" endpoint
data SubmitDidParams = SubmitDidParams
    { 
      submittedDid :: String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "claimDid" endpoint
data ClaimDidParams = ClaimDidParams
    { claimedDid :: String,
      claimCode :: String 
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | Pair of (did, code), which allow to find 
-- | TODO: check for overflow. (persistent).
-- | Currently this is toy implementation, real with the same interface
-- | to the blockchain will be in other language.
data State = CmdMapSet (Map String String) 
              |
             CmdMapPut String String
              |
             CmdMapRemove String
               deriving stock (Eq, Show, Generic)
               deriving anyclass (FromJSON, ToJSON)

instance Semigroup State where
   a <> b =
      let r = case a of 
               CmdMapSet ma ->
                 case b of 
                     CmdMapSet mb ->
                        Map.union ma mb
                     CmdMapPut kb vb ->
                        Map.insert kb vb ma
                     CmdMapRemove kb ->
                        Map.delete kb ma
               CmdMapPut ka va  ->
                 case b of 
                     CmdMapSet mb ->
                        Map.insert ka va mb
                     CmdMapPut kb vb ->
                        Map.insert ka va (Map.singleton kb vb)
                     CmdMapRemove kb ->
                        if (ka == kb) then Map.empty else (Map.singleton ka va)
               CmdMapRemove ka ->
                 case b of 
                     CmdMapSet mb ->
                        Map.delete ka mb
                     CmdMapPut kb vb ->
                        if (ka == kb) then Map.empty else (Map.singleton kb vb)
                     CmdMapRemove kb ->
                        Map.empty
      in (CmdMapSet r)
                 


instance Monoid State where
   mempty = (CmdMapSet Map.empty)
                 

-- | The "submitDid" contract endpoint. See note [Contract endpoints]
-- | Actually, this code will be implemented not in pub,
-- | Because it should come from user address, so submit did endpoint
-- | Is unaviable for him.
-- | But we will leave imlementation here
submitDid :: AsContractError e => Promise State DidAddressSchema e ()
submitDid = endpoint @"submitDid" @SubmitDidParams $ \(SubmitDidParams did) -> do
    let code :: String =  "1234" -- | TODO: generate random code and send to DID
    -- here we assume that contract state is not available from wallet.
    -- this can be incorrect for inbrowser wallets, so in real life, better not to choose 
    -- onchain Haskell.  In hosted node it works safely, but hosted mode is near useless in practice.
    tell (CmdMapPut did code)
    logInfo @String $ "Submit " <> show did <> " to the script"

convertDidDatum :: String -> String -> DidDatum
convertDidDatum did code = (DidDatum (stringToBuiltinByteString did) (stringToBuiltinByteString code))


-- | The "claimDid" contract endpoint. See note [Contract endpoints]
-- | This code is called, when 
claimDid :: AsContractError e => Promise State DidAddressSchema e ()
claimDid = endpoint @"claimDid" @ClaimDidParams $ \(ClaimDidParams did code) -> do
    -- logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq didAddressAddress (Ada.lovelaceValueOf 1)

    --
    --let redeemer = hashString code
    --    tx       = collectFromScript utxos redeemer

    -- In a real use-case, we would not submit the transaction if we know that code is invalie
    -- wrong.
    logInfo @String "Submitting transaction to claim did"
    let minAmount = Ada.lovelaceValueOf 1
    let didDatum  = convertDidDatum did code
    let tx = Constraints.mustPayToTheScript didDatum minAmount
    void (submitTxConstraintsSpending didAddressInstance utxos tx)

didAddress :: AsContractError e => Contract State DidAddressSchema e ()
didAddress = do
    logInfo @String "Waiting for guess or lock endpoint..."
    selectList [submitDid, claimDid] >> didAddress

