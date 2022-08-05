{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
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
{-# LANGUAGE ScopedTypeVariables #-}


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
import           System.Random         
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


class StateCommand s c | s->c where
    initState :: s
    apply :: c->s->s
    merge :: s->s->s
    
data StateOrCommand s c  =  SetState s
                           |
                            Command c
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

 
instance (StateCommand s c) => Semigroup (StateOrCommand s c) where
   a <> b =
     let sa = case a of
               SetState sa -> sa
               Command ca -> apply ca initState
     in
       let r = case b of
               SetState sb -> merge sa sb
               Command cb -> apply cb sa
       in (SetState r)

instance (StateCommand s c) => Monoid (StateOrCommand s c) where
    mempty = SetState initState

data MapCommand = PutRandom String String -- intercepted by state update effect and generate random value
                     |
                      Put String String  -- add key and valut to map. internal, result of interception.
                     |
                      Delete String
                    deriving stock (Eq, Show, Generic)
                    deriving anyclass (FromJSON, ToJSON)

instance StateCommand (Map String String) MapCommand where
       initState = Map.empty
       apply c s = case c of
                     PutRandom did entropy -> Map.insert did entropy s -- will be never called when random effect is enabled.
                     Put did code -> Map.insert did code s
                     Delete did -> Map.delete did s
       merge x y = Map.union x y



type State = StateOrCommand (Map String String) MapCommand
                 

-- | The "submitDid" contract endpoint. See note [Contract endpoints]
-- | Actually, this code will be implemented not in pub,
-- | Because it should come from user address, so submit did endpoint
-- | Is unaviable for him.
-- | But we will leave imlementation here
submitDid :: AsContractError e => Promise State DidAddressSchema e ()
submitDid = endpoint @"submitDid" @SubmitDidParams $ \(SubmitDidParams did) -> do
    let entropy :: String =  "1234" -- | TODO: generate random code and send to DID
    -- here we assume that contract state is not available from wallet.
    -- this can be incorrect for inbrowser wallets, so in real life, better not to choose 
    -- onchain Haskell.  In hosted node it works safely, but hosted mode is near useless in practice.
    tell (Command (PutRandom did entropy))
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

--TODO: add endpoint which should be called by service owner.

didAddress :: AsContractError e => Contract State DidAddressSchema e ()
didAddress = do
    logInfo @String "Waiting for guess or lock endpoint..."
    selectList [submitDid, claimDid] >> didAddress

