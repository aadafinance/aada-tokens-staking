{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module LockFunds
  ( lockFunds
  , lockFundsShortBs
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


data LockFundsDatum = LockFundsDatum
    { beneficiary :: !PubKeyHash
    }

data ContractInfo = ContractInfo
    { secretHash    :: !DatumHash
    , aadaScAddrPkh :: !ValidatorHash
    }

contractInfo = ContractInfo
    { secretHash = "ff"
    , aadaScAddrPkh = "e8e5f8aa6b99363f39a7d8883652e257be3a7311c908da9ab8116bb5"
    }

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> LockFundsDatum -> () -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat _ ctx =
    if isItEmergencyClaim
        then traceIfFalse "Emergency claim failed" validateEmergencyClaim
        else traceIfFalse "Validation failed" validate
  where
    findTxFromThisSc :: Maybe TxOut
    findTxFromThisSc = case findOwnInput ctx of
      Just txInInfo -> Just (txInInfoResolved txInInfo)
      Nothing       -> Nothing

    isItEmergencyClaim :: Bool
    isItEmergencyClaim = case findTxFromThisSc of
      Just txo -> traceIfTrue "Emergency claim!!!" (txOutDatumHash txo == secretHash)
      Nothing  -> False

    checkTxToAadaScAadr :: TxOut -> Bool
    checkTxToAadaScAadr tx = case toValidatorHash $ txOutAddress tx of
      Just Hash -> aadaScAddrPkh == hash
      Nothing   -> False

    validateEmergencyClaim :: Bool
    validateEmergencyClaim = case findTxFromThisSc of
      Just txo -> traceIfFalse "Emergency claim tx not being sent to emergency claim sc" (checkTxToAadaScAadr txo)
      Nothing  -> traceIfFalse "transaction input from this sc not found" False

    txToDatAddr :: TxOut -> Bool
    txToDatAddr txo = case pubKeyOutput txo of
      Just pkh -> beneficiary dat == pkh
      Nothing  -> False

    validate :: Bool
    validate = case findTxFromThisSc of
      Just txo -> traceIfFalse "transaction is not being sent to beneficiary provided in datum" (txToDatAddr txo)
      Nothing  -> traceIfFalse "transaction input from this sc not found" False

data LockFunds
instance Scripts.ValidatorTypes LockFunds where
    type instance DatumType LockFunds = LockFundsDatum
    type instance RedeemerType LockFunds = ()

typedValidator :: Scripts.TypedValidator LockFunds
typedValidator = Scripts.mkTypedValidator @LockFunds
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @LockFundsDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo
PlutusTx.makeIsDataIndexed ''LockFundsDatum [('LockFundsDatum, 0)]

script :: Plutus.Script
script = Plutus.unValidatorScript validator

lockFundsShortBs :: SBS.ShortByteString
lockFundsShortBs = SBS.toShort . LBS.toStrict $ serialise script

lockFunds :: PlutusScript PlutusScriptV1
lockFunds = PlutusScriptSerialised lockFundsShortBs
