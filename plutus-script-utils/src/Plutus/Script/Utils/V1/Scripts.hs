{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{- |
This module contains functions related to the computation of script hashes for
PlutusV1.
-}
module Plutus.Script.Utils.V1.Scripts (
  -- * Script hashes
  PV1.Validator,
  PV1.ValidatorHash,
  PV1.MintingPolicy,
  PV1.MintingPolicyHash,
  PV1.StakeValidator,
  PV1.StakeValidatorHash,
  PV1.fromCardanoHash,
  validatorHash,
  mintingPolicyHash,
  stakeValidatorHash,
  scriptHash,

  -- * Script utilities
  scriptCurrencySymbol,
  toCardanoApiScript,
) where

import Cardano.Api qualified as Script
import Cardano.Api.Shelley qualified as Script
import Plutus.Script.Utils.Scripts qualified as PV1
import PlutusLedgerApi.V1 qualified as PV1

-- | Hash a 'PV1.Validator' script.
validatorHash :: PV1.Validator -> PV1.ValidatorHash
validatorHash =
  PV1.ValidatorHash
    . PV1.getScriptHash
    . scriptHash
    . PV1.getValidator

-- | Hash a 'PV1.MintingPolicy' script.
mintingPolicyHash :: PV1.MintingPolicy -> PV1.MintingPolicyHash
mintingPolicyHash =
  PV1.MintingPolicyHash
    . PV1.getScriptHash
    . scriptHash
    . PV1.getMintingPolicy

-- | Hash a 'PV1.StakeValidator' script.
stakeValidatorHash :: PV1.StakeValidator -> PV1.StakeValidatorHash
stakeValidatorHash =
  PV1.StakeValidatorHash
    . PV1.getScriptHash
    . scriptHash
    . PV1.getStakeValidator

-- | Hash a 'Script'
scriptHash :: PV1.Script -> PV1.ScriptHash
scriptHash =
  PV1.fromCardanoHash
    . Script.hashScript
    . toCardanoApiScript

{- | Convert a 'Script' to a 'cardano-api' script.

For why we depend on `cardano-api`,
see note [Hash computation of datums, redeemers and scripts]
-}
toCardanoApiScript :: PV1.Script -> Script.Script Script.PlutusScriptV1
toCardanoApiScript =
  Script.PlutusScript Script.PlutusScriptV1
    . Script.PlutusScriptSerialised
    . PV1.unScript

{-# INLINEABLE scriptCurrencySymbol #-}

-- | The 'CurrencySymbol' of a 'MintingPolicy'.
scriptCurrencySymbol :: PV1.MintingPolicy -> PV1.CurrencySymbol
scriptCurrencySymbol scrpt =
  let (PV1.MintingPolicyHash hsh) = mintingPolicyHash scrpt in PV1.CurrencySymbol hsh
