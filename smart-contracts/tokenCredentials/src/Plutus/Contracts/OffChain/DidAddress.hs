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
    -- TODO: finalizePublish
    , DidAddressSchema
    , SubmitDidParams
    , ClaimDidParams
    , TCProxyGetSubmittedDidsParams
    ) where

import           Control.Monad         (void)
import qualified Data.Aeson
import           Data.Aeson           
import qualified Data.Aeson.Types
import qualified Data.Dequeue          as Dequeue
import           Data.Dequeue          (BankersDequeue, Dequeue)
import qualified Data.Foldable         as Foldable
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
import           Prelude             
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import           Plutus.Contracts.OnChain.DidAddress 
import           Plutus.Contracts.OnChain.DidAddress (DidDatum (..))


type DidAddressSchema =
        Endpoint "submitDid" SubmitDidParams
        .\/ Endpoint "claimDid" ClaimDidParams
        .\/ Endpoint "tcproxyGetSumbittedDids" TCProxyGetSubmittedDidsParams

-- Endpoint params

data SubmitDidParams = SubmitDidParams
    { 
      submittedDid :: String
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data ClaimDidParams = ClaimDidParams
    { claimedDid :: String,
      claimCode :: String 
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data TCProxyGetSubmittedDidsParams = TCProxyGetSubmittedDidsParams 
                                      { maxItems:: Int }
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


-- TODO: add TTL
data OnlyState = OnlyState {
    didCodeMap :: Map String String,
    didWithoutCodes :: BankersDequeue String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


instance (FromJSON t) => FromJSON(BankersDequeue t) where
    parseJSON = fmap Dequeue.fromList . parseJSON
    
instance (ToJSON t) => ToJSON(BankersDequeue t) where
    toJSON v = toJSON (Foldable.toList v)


data OnlyCommand =    EnqueueMessage String 
                     |
                      DequeueMessages Int
                     |
                      Put String String  
                     |
                      Delete String
                    deriving stock (Eq, Show, Generic)
                    deriving anyclass (FromJSON, ToJSON)

instance StateCommand OnlyState OnlyCommand where
       initState = OnlyState Map.empty Dequeue.empty
       apply c s = case c of
                     EnqueueMessage did -> s{ didWithoutCodes = Dequeue.pushBack (didWithoutCodes s) did }
                     DequeueMessages n -> s{ didWithoutCodes = popFrontN (didWithoutCodes s) n }
                                            where 
                                              popFrontN :: (BankersDequeue t) -> Int -> BankersDequeue t
                                              popFrontN dq n = 
                                                 if (n == 0) then dq
                                                 else case Dequeue.popFront dq of
                                                        Nothing -> dq
                                                        Just (v, dq') -> popFrontN dq' (n-1)
                     Put did code -> s{ didCodeMap = Map.insert did code (didCodeMap s) }
                     Delete did -> s{ didCodeMap = Map.delete did (didCodeMap s) }
       merge x y = OnlyState{    
                      didCodeMap = Map.union (didCodeMap x) (didCodeMap y),
                      didWithoutCodes = Dequeue.fromList (
                                  (Foldable.toList (didWithoutCodes x)) ++
                                  (Foldable.toList (didWithoutCodes y))
                      )
                   }


type State = StateOrCommand OnlyState OnlyCommand
                 

-- | The "submitDid" contract endpoint. See note [Contract endpoints]
-- | Actually, this code will be implemented not in pub,
-- | Because it should come from user address, so submit did endpoint
-- | Is unaviable for him.
-- | But we will leave imlementation here
submitDid :: AsContractError e => Promise State DidAddressSchema e ()
submitDid = endpoint @"submitDid" @SubmitDidParams $ \(SubmitDidParams did) -> do
    tell (Command (EnqueueMessage did))
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

