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

module Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies (
  mkForwardingMintingPolicy,
  forwardToValidator,
) where

import Plutus.Script.Utils.Scripts (
  MintingPolicy,
  ValidatorHash (ValidatorHash),
  mkMintingPolicyScript,
 )
import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 (
  Address (Address, addressCredential),
  Credential (ScriptCredential),
  ScriptHash (ScriptHash),
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxInfo (TxInfo, txInfoInputs),
 )
import PlutusLedgerApi.V2.Contexts qualified as PV2
import PlutusLedgerApi.V2.Tx (TxOut (TxOut, txOutAddress))
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False), any, ($), (.), (==))

-- TODO: we should add a TypedMintingPolicy interface here

{- | A minting policy that checks whether the validator script was run
  in the minting transaction.
-}
mkForwardingMintingPolicy :: ValidatorHash -> MintingPolicy
mkForwardingMintingPolicy vshsh =
  mkMintingPolicyScript
    $ $$( PlutusTx.compile
            [||
            \(hsh :: ValidatorHash) ->
              mkUntypedMintingPolicy (forwardToValidator hsh)
            ||]
        )
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 vshsh

{-# INLINEABLE forwardToValidator #-}
forwardToValidator :: ValidatorHash -> () -> PV2.ScriptContext -> Bool
forwardToValidator (ValidatorHash h) _ ScriptContext{scriptContextTxInfo = TxInfo{txInfoInputs}, scriptContextPurpose = Minting _} =
  let checkHash TxOut{txOutAddress = Address{addressCredential = ScriptCredential (ScriptHash vh)}} = vh == h
      checkHash _ = False
   in any (checkHash . PV2.txInInfoResolved) txInfoInputs
forwardToValidator _ _ _ = False
