{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCArds #-}

module LockFundsv2
  ( lockfundsv2 , lockfundsv2ShortBs
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Crypto as Crypto
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Address
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude              (IO, Semigroup (..), Show (..), String)

import           Ledger.Typed.Scripts as Scripts


data LockFundsv2Redeemer = LockFundsv2Redeemer
    { num1 :: Integer
    , num2 :: Integer
    }

data ContractInfo = ContractInfo
    { secretHash          :: !DatumHash
    , aaadaEmergencyScPkh :: !ValidatorHash
    , aadaPkh             :: !PubKeyHash
    }

contractInfo = ContractInfo
    { secretHash ="ff"
    , aadaEmergencyScPkh = "e8e5f8aa6b99363f39a7d8883652e257be3a7311c908da9ab8116bb5"
    , aadaPkh = "ff"
    , carmichel = 123412341234
    }
    

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> () -> LockFundsv2Redeemer -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} () rdm ctx = validate
  if isItEmergencyClaim
    then traceIfFalse "Emergency claim failed" validateEmergencyClaim
    else traceIfFalse "Validation failed"      validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    findTxFromThisSc :: Maybe TxOut
    findTxFromThisSc = case findOwnInput ctx of
      Just txInInfo -> Just (txInInfoResolved txInInfo)
      Nothing       -> Nothing

    isItEmergencyClaim :: Bool
    isItEmergencyClaim = case findTxFromThisSc of
      Just txo -> traceIfTrue "Emergency Claim!!!" (txOutDatumHash txo == secretHash)
      Nothing  -> False

    checkTxToAadaScAadr :: TxOut -> Bool
    checkTxToAadaScAadr tx = case toValidatorHash $ txOutAddress tx of
      Just Hash -> aadaEmergencyScPkh == hash
      Nothing   -> False

    validateEmergencyClaim :: Bool
    validateEmergencyClaim = case findTxFromThisSc of
      Just txo -> traceIfFalse "Emergency claim tx not being sent to emergency claim sc" (checkTxToAadaScAadr txo)
      Nothing  -> traceIfFalse "transaction input from this sc not found" False

    txSignedByAada :: Bool
    txSignedByAada = traceIfFalse "Transaction wasn't signed by aada backend" (txSignedBy info aadaPkh)

    correctKeysProvided :: Bool
    correctKeysProvided = 
      traceIfFalse "Wrong redeemer guess numbers"
      ((rdm num1 - 1) * (rdm num2 - 1) == carmichel)

    validate :: Bool
    validate = txSignedByAada && correctKeysProvided

data LockFundsv2
instance Scripts.ValidatorTypes LockFundsv2 where
    type instance DatumType LockFundsv2 = ()
    type instance RedeemerType LockFundsv2 = LockFundsv2Redeemer

typedValidator :: Scripts.TypedValidator LockFundsv2
typedValidator = Scripts.mkTypedValidator @LockFundsv2
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @LockFundsv2Redeemer 

validator :: Validator
validator = Scripts.validatorScript typedValidator

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo
PlutusTx.makeIsDataIndexed ''LockFundsv2Redeemer [('LockFundsv2Redeemer, 0)]

script :: Plutus.Script
script = Plutus.unValidatorScript validator

lockfundsv2ShortBs :: SBS.ShortByteString
lockfundsv2ShortBs = SBS.toShort . LBS.toStrict $ serialise script

lockfundsv2 :: PlutusScript PlutusScriptV1
lockfundsv2 = PlutusScriptSerialised lockfundsv2ShortBs
