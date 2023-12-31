{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Script.Utils.V1.Typed.Scripts.StakeValidators (
  mkForwardingStakeValidator,
  forwardToValidator,
) where

import Plutus.Script.Utils.Scripts (
  StakeValidator,
  ValidatorHash (ValidatorHash),
  mkStakeValidatorScript,
 )
import Plutus.Script.Utils.Typed (mkUntypedStakeValidator)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (
  Address (Address, addressCredential),
  Credential (ScriptCredential),
  ScriptHash (ScriptHash),
 )
import PlutusLedgerApi.V1.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Certifying, Rewarding),
  TxInfo (TxInfo, txInfoInputs),
 )
import PlutusLedgerApi.V1.Contexts qualified as PV1
import PlutusLedgerApi.V1.Tx (TxOut (TxOut, txOutAddress))
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False), any, ($), (.), (==))

-- TODO: we should add a TypedStakeValidator interface here

{- | A stake validator that checks whether the validator script was run
  in the right transaction.
-}
mkForwardingStakeValidator :: ValidatorHash -> StakeValidator
mkForwardingStakeValidator vshsh =
  mkStakeValidatorScript
    $ $$( PlutusTx.compile
            [||
            \(hsh :: ValidatorHash) ->
              mkUntypedStakeValidator (forwardToValidator hsh)
            ||]
        )
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 vshsh

{-# INLINEABLE forwardToValidator #-}
forwardToValidator :: ValidatorHash -> () -> ScriptContext -> Bool
forwardToValidator (ValidatorHash h) _ ScriptContext{scriptContextTxInfo = TxInfo{txInfoInputs}, scriptContextPurpose} =
  let checkHash TxOut{txOutAddress = Address{addressCredential = ScriptCredential (ScriptHash vh)}} = vh == h
      checkHash _ = False
      result = any (checkHash . PV1.txInInfoResolved) txInfoInputs
   in case scriptContextPurpose of
        Rewarding _ -> result
        Certifying _ -> result
        _ -> False
