{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskbucks.Event
( EventLog(..)
, runState
, withStateEvents
, runStm
, withStmEvents
, runPg
, withPgEvents
, dropPgEvents
) where

import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (MonadState, State)
import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import           Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Data.Aeson as JSON
import           Data.Typeable (Typeable)
import           Data.ByteString (ByteString)
import Data.Hashable (hash)
import Control.Monad
import Control.Exception (bracket)

data EventLog ev m = EventLog {
  snapshot :: m [ev]
, append :: ev -> m ()
}

data PgLogItem ev = PgLogItem {
  _lId :: Integer,
  lValue :: ev
} deriving (Show, Typeable)

instance (Typeable ev, JSON.FromJSON ev) => Pg.FromRow (PgLogItem ev) where
  fromRow = PgLogItem <$> Pg.field <*> Pg.fieldWith Pg.fromJSONField


runState :: State [ev] a -> IO a
runState = pure . flip State.evalState []

withStateEvents :: (EventLog ev (State [ev]) -> IO a) -> IO a
withStateEvents f = do
  f stateLogger
  where
  stateLogger :: MonadState [a] m => EventLog a m
  stateLogger = EventLog getSnapshot writer
    where
    getSnapshot = State.get
    writer ev = State.modify $ (++ [ev])

runStm :: STM a -> IO a
runStm = STM.atomically

withStmEvents :: (EventLog ev STM -> IO a) -> IO a
withStmEvents f = do
  events <- STM.atomically $ newStmEvents
  f events
  where
  newStmEvents :: STM (EventLog a STM)
  newStmEvents = do
    evVar <- STM.newTVar []

    let getSnapshot = STM.readTVar evVar
    let writer ev = STM.modifyTVar' evVar (++ [ev])

    pure $ EventLog getSnapshot writer


withPgEvents :: (Typeable ev, JSON.ToJSON ev, JSON.FromJSON ev) => ByteString -> (EventLog ev IO -> IO ()) -> IO ()
withPgEvents url f = do

      bracket (newPgPool url) (Pool.destroyAllResources) $ \pool -> do
        dropPgEvents pool
        evs <- newPgEvents pool
        f $ evs

newPgPool :: ByteString -> IO (Pool Pg.Connection)
newPgPool url = do
  pool <- Pool.createPool (Pg.connectPostgreSQL url) Pg.close 1 60 5
  pure pool

newPgEvents :: forall ev . (Typeable ev, JSON.ToJSON ev, JSON.FromJSON ev) => Pool Pg.Connection -> IO (EventLog ev IO)
newPgEvents pool = do
  Pool.withResource pool $ setupDb
  pure $ EventLog getSnapshot writer
  where
  getSnapshot = Pool.withResource pool $ \c -> do
      rows <- Pg.query_ c "select * from coffee_logs"
      pure $ fmap lValue rows

  writer ev = Pool.withResource pool $ \c -> do
    _ <- Pg.execute c "insert into coffee_logs (value) values (?);" $ Pg.Only (Pg.toJSONField ev)
    pure ()

  setupDb c = do
    Pg.withTransaction c $ do
      _ <- Pg.query c "select pg_advisory_xact_lock(?);" (Pg.Only tableHash) :: IO [Pg.Only ()]
      void $ Pg.execute_ c "CREATE TABLE IF NOT EXISTS coffee_logs (id serial primary key, value jsonb);"

  tableHash = hash ("coffee_logs" :: String)

dropPgEvents :: Pool Pg.Connection -> IO ()
dropPgEvents pool = do
  Pool.withResource pool $ \c -> do
    void $ Pg.execute_ c "DROP TABLE IF EXISTS coffee_logs;"

runPg :: IO a -> IO a
runPg = id
