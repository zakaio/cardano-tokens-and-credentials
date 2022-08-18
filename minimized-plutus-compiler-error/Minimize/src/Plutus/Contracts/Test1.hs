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
{-# LANGUAGE BangPatterns               #-}


-- | Receive 
module Plutus.Contracts.Test1
    ( 
      SubmitDidDatum (..)
    -- * Scripts
      ,didAddressMintingPolicy
      ,didAddressTokenCurrency
    ) where

import           Plutus.V1.Ledger.Api  (CurrencySymbol (..), 
                                        Datum (..), 
                                        ScriptContext,  
                                        PubKeyHash,
                                        MintingPolicy (..), 
                                        mkMintingPolicyScript)
import           Plutus.V1.Ledger.Scripts      (ScriptHash (..))
import qualified Ledger
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V1.Scripts as Scripts


data SubmitDidDatum = SubmitDidDatum {
      dtSubmitDid :: !BuiltinByteString,
      dtSubmitPkh :: !PubKeyHash
   } 

--   deriving anyclass (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.unstableMakeIsData ''SubmitDidDatum

PlutusTx.makeLift ''SubmitDidDatum


-- ! Minting Policy
--

{-# INLINABLE didAddressMintingPolicy #-}
didAddressMintingPolicy :: PubKeyHash -> PubKeyHash -> BuiltinByteString -> BuiltinData -> BuiltinData -> ()
didAddressMintingPolicy  contractOwner  didOwner did  _  ctxData =
              if (not (Ledger.txSignedBy txInfo contractOwner) ) then
                 error () 
              else if ( isNothing( find (\x -> (snd x)==foundDatum)  txInfoData ) ) then
                 error () 
              else
                 ()
              where
                ctx :: ScriptContext = PlutusTx.unsafeFromBuiltinData ctxData
                txInfoData = (Ledger.txInfoData txInfo)
                txInfo = (Ledger.scriptContextTxInfo ctx)
                -- --works:
                --foundDatum = Datum ( PlutusTx.toBuiltinData  datumString  )
                --datumString = did <> (getPubKeyHash didOwner)
                foundDatum = Datum datumData
                datumData = PlutusTx.toBuiltinData submitDatum
                submitDatum = SubmitDidDatum did didOwner
                
           

{-# INLINABLE didAddressMintingPolicyScript #-}
didAddressMintingPolicyScript :: PubKeyHash -> PubKeyHash -> BuiltinByteString -> Scripts.MintingPolicy
didAddressMintingPolicyScript contractOwner didOwner did =
           mkMintingPolicyScript
             ($$(PlutusTx.compile [|| didAddressMintingPolicy ||])
                   `PlutusTx.applyCode` PlutusTx.liftCode contractOwner
                   `PlutusTx.applyCode` PlutusTx.liftCode didOwner
                   `PlutusTx.applyCode` PlutusTx.liftCode did
             )

{-# INLINABLE didAddressTokenCurrency #-}
didAddressTokenCurrency :: PubKeyHash -> PubKeyHash -> BuiltinByteString -> CurrencySymbol
didAddressTokenCurrency    contractOwner  didOwner did  =
                CurrencySymbol (getScriptHash  sh)
             where
               sh = Scripts.scriptHash script
               script = getMintingPolicy (didAddressMintingPolicyScript contractOwner didOwner did) 


