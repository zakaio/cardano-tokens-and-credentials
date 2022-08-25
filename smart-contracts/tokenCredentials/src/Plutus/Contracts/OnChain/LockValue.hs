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


--
--  The idea --- somebody (did owner)
--  send value to the lock contract
--  with own did
--
--  tcproxy send to this did credential
--  with utxo id and unlock key.
--
--  user can use this credential and
--   send unlock key to credential verifier
--   who will use unlocked value.
--
-- (TODO: think about situation when user lost credential.
---       maybe prepare version without datum for this)
--
module Plutus.Contracts.OnChain.LockValue(
           LockValueParams (..),
           LockValueDatum (..),
           lockValueValidator,
           lockValueAddress,
           validateLockValueInstance
) where



import qualified Ledger
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell
import           Plutus.V1.Ledger.Api  (Address,
                                        DatumHash (..), Datum (..), 
                                        PubKeyHash, 
                                        POSIXTime,
                                        ScriptContext, 
                                        Validator,
                                        mkValidatorScript)
import qualified Plutus.V1.Ledger.Interval            as Interval
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts

data LockValueParams = LockValueParams {
     lvpExpireTime:: POSIXTime
}


PlutusTx.makeLift ''LockValueParams

-- standard lock contract.
newtype LockValueDatum = LockValueDatum {
     lvdCodeString:: BuiltinByteString      
} 

PlutusTx.unstableMakeIsData ''LockValueDatum
PlutusTx.makeLift ''LockValueDatum


data LockValue
instance Scripts.ValidatorTypes LockValue where
    type instance RedeemerType LockValue = LockValueDatum
    type instance DatumType LockValue = LockValueDatum

-- use untyped interface for V2 because typed yet not available.
validateLockValueUntyped :: LockValueParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
validateLockValueUntyped params datum redeemer context = 
     if (condition) then
          ()
     else
          error()
     where     
          condition = validateLockValueTyped params
                            (PlutusTx.unsafeFromBuiltinData datum) 
                            (PlutusTx.unsafeFromBuiltinData redeemer) 
                            (PlutusTx.unsafeFromBuiltinData context)


-- this is the minimal lock script.
-- the first minimal is always success, because code-string should be send with datum
validateLockValueTyped :: LockValueParams -> LockValueDatum -> LockValueDatum -> ScriptContext -> Bool
validateLockValueTyped params datum redeemer ctx = 
     if (Interval.after (lvpExpireTime params) (Ledger.txInfoValidRange txInfo)) then
          -- only return allow to previous address
          isJust (find (\x -> (Ledger.txOutAddress (Ledger.txInInfoResolved x)) == currentOutAddress) (Ledger.txInfoInputs txInfo) )
     else
          --  'true'because user ownity should know datum 
          (lvdCodeString redeemer) == (lvdCodeString datum)
     where
          currentOutAddress = Ledger.txOutAddress currentOut
          currentOut::Ledger.TxOut = case (Ledger.txInfoOutputs txInfo) of
                                        (x:xs) -> x
                                        [] -> error() 
          txInfo = (Ledger.scriptContextTxInfo ctx)


validateLockValueInstance :: LockValueParams -> Scripts.TypedValidator LockValue
validateLockValueInstance params = Scripts.mkTypedValidator @LockValue
    ($$(PlutusTx.compile [|| validateLockValueTyped ||])
            `PlutusTx.applyCode` PlutusTx.liftCode params
    )
    $$(PlutusTx.compile [|| wrap ||]) 
     where
        wrap = Scripts.mkUntypedValidator @LockValueDatum @LockValueDatum

lockValueValidator :: LockValueParams -> Validator 
lockValueValidator params = Scripts.validatorScript (validateLockValueInstance params)

lockValueAddress :: LockValueParams -> Address
lockValueAddress params = Ledger.scriptAddress (lockValueValidator params)


    