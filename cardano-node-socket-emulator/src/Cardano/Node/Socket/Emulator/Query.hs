{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Node.Socket.Emulator.Query (handleQuery) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Concurrent (MVar, readMVar)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.SOP (K (K))
import Data.SOP.Strict (NS (S, Z))
import Ledger.Tx.CardanoAPI (fromPlutusIndex)
import Ouroboros.Consensus.Cardano.Block (BlockQuery (..), CardanoBlock, CardanoQueryResult)
import Ouroboros.Consensus.HardFork.Combinator (QueryHardFork (..))
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Shelley.Eras (BabbageEra, StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.Query (BlockQuery (..))
import Ouroboros.Network.Block qualified as O

import Cardano.Ledger.UTxO qualified as Ledger
import Cardano.Node.Emulator.API qualified as E
import Cardano.Node.Emulator.Internal.Node.Params (
  Params (..),
  emulatorEraHistory,
  genesisDefaultsFromParams,
 )
import Cardano.Node.Emulator.Internal.Node.TimeSlot (posixTimeToUTCTime, scSlotZeroTime)
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  getTip,
  runChainEffects,
 )
import Ledger.Tx (toCtxUTxOTxOut)
import Ouroboros.Consensus.Protocol.Praos (Praos)

handleQuery
  :: (block ~ CardanoBlock StandardCrypto)
  => MVar AppState
  -> Query block result
  -> IO result
handleQuery state = \case
  BlockQuery (QueryIfCurrentBabbage q) -> queryIfCurrentBabbage state q
  BlockQuery (QueryHardFork GetInterpreter) -> do
    AppState _ _ params <- readMVar state
    let C.EraHistory interpreter = emulatorEraHistory params
    pure interpreter
  BlockQuery (QueryHardFork GetCurrentEra) -> do
    pure $ Consensus.EraIndex (S (S (S (S (S (Z (K ()))))))) -- BabbageEra
  BlockQuery q -> printError $ "Unimplemented BlockQuery received: " ++ show q
  GetSystemStart -> do
    AppState _ _ Params{pSlotConfig} <- readMVar state
    pure $ C.SystemStart $ posixTimeToUTCTime $ scSlotZeroTime pSlotConfig
  GetChainBlockNo -> do
    tip <- getTip state
    case tip of
      O.TipGenesis -> pure Origin
      (O.Tip _ _ curBlockNo) -> pure $ At curBlockNo
  GetChainPoint -> printError "Unimplemented: GetChainPoint"

queryIfCurrentBabbage
  :: (block ~ Shelley.ShelleyBlock (Praos StandardCrypto) (BabbageEra StandardCrypto))
  => MVar AppState
  -> BlockQuery block result
  -> IO (CardanoQueryResult StandardCrypto result)
queryIfCurrentBabbage state = \case
  GetGenesisConfig -> do
    AppState _ _ params <- readMVar state
    pure $ Right $ Shelley.compactGenesis $ genesisDefaultsFromParams params
  GetCurrentPParams -> do
    AppState _ _ params <- readMVar state
    pure $ Right $ emulatorPParams params
  GetUTxOByAddress addrs -> do
    (_logs, res) <- runChainEffects state $ do
      utxos <- traverse (E.utxosAt . C.fromShelleyAddrIsSbe C.shelleyBasedEra) $ toList addrs
      pure $ fromPlutusIndex (mconcat utxos)
    either (printError . show) (pure . Right) res
  GetUTxOByTxIn txIns -> do
    (_logs, res) <- runChainEffects state $ do
      utxos <-
        traverse
          ( \txIn ->
              fmap ((txIn,) . C.toShelleyTxOut C.shelleyBasedEra . toCtxUTxOTxOut)
                <$> E.utxoAtTxOutRef (C.fromShelleyTxIn txIn)
          )
          $ toList txIns
      pure $ Ledger.UTxO $ Map.fromList $ catMaybes utxos
    either (printError . show) (pure . Right) res
  GetStakePools -> do
    pure $ Right mempty
  q -> printError $ "Unimplemented BlockQuery(QueryIfCurrentBabbage) received: " ++ show q

printError :: String -> IO a
printError s = print s >> error s
