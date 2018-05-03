{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskbucks.Event
( EventLog(..)
, runState
, withStateEvents
, runStm
, withStmEvents
, runPg
, withPgPool
, withPgEvents
, dropPgEvents
) where

import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (State)
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
import Streaming
import qualified Streaming.Prelude as S
import Control.Monad.Trans.Resource


data EventLog ev m = EventLog {
  snapshot :: m [ev]
, append :: ev -> m ()
  -- Argh. What we principally need here, is a way to get everything upto _and
  -- including_ the current head (for cases where we need the current state),
  -- vs fetching everything for ever more.
  --
  -- We could add a _current head_ method that'll effectively form a snapshot?
  -- Or; fold until we; erm, something.

, stream :: Stream (Of ev) m ()
}

data PgLogItem ev = PgLogItem {
  _lId :: Integer,
  lValue :: ev
} deriving (Show, Typeable)

instance (Typeable ev, JSON.FromJSON ev) => Pg.FromRow (PgLogItem ev) where
  fromRow = PgLogItem <$> Pg.field <*> Pg.fieldWith Pg.fromJSONField


runState :: State [ev] a -> IO a
runState = pure . flip State.evalState []

withStateEvents :: forall ev a . (EventLog ev (State [ev]) -> IO a) -> IO a
withStateEvents f = do
  f stateLogger
  where
  stateLogger = EventLog getSnapshot writer consumer
    where
    getSnapshot = State.get

    consumer  :: Stream (Of ev) (State [ev]) ()
    consumer = do
      evs <- lift State.get
      mapM_ S.yield evs

    writer ev = State.modify $ (++ [ev])

runStm :: STM a -> IO a
runStm = STM.atomically

withStmEvents :: forall ev a . (EventLog ev STM -> IO a) -> IO a
withStmEvents f = do
  events <- STM.atomically $ newStmEvents
  f events
  where
  newStmEvents :: STM (EventLog ev STM)
  newStmEvents = do
    evVar <- STM.newTVar []

    let getSnapshot = STM.readTVar evVar
    let consumer = do
                  evs <- lift $ STM.readTVar evVar :: Stream (Of ev) STM [ev]
                  mapM_ S.yield evs :: Stream (Of ev) STM ()

    let writer ev = STM.modifyTVar' evVar (++ [ev])

    pure $ EventLog getSnapshot writer consumer


type RIO = (ResourceT IO)
withPgPool :: ByteString -> (Pool Pg.Connection -> IO a) -> IO a
withPgPool url = bracket (newPgPool url) (Pool.destroyAllResources)

withPgEvents :: (Typeable ev, JSON.ToJSON ev, JSON.FromJSON ev) => Pool Pg.Connection -> (EventLog ev RIO -> IO ()) -> IO ()
withPgEvents pool f = do
    evs <- newPgEvents pool
    f $ evs

newPgPool :: ByteString -> IO (Pool Pg.Connection)
newPgPool url = do
  pool <- Pool.createPool (Pg.connectPostgreSQL url) Pg.close 1 60 5
  pure pool

newPgEvents :: forall ev . (Typeable ev, JSON.ToJSON ev, JSON.FromJSON ev) => Pool Pg.Connection -> IO (EventLog ev RIO)
newPgEvents pool = do
  Pool.withResource pool $ setupDb
  pure $ EventLog getSnapshot writer consumer
  where
  getSnapshot = lift $ Pool.withResource pool $ \c -> do
      rows <- Pg.query_ c "select * from coffee_logs"
      pure $ fmap lValue rows

  consumer = do
      (key, c) <- lift $ borrowConn
      rows <- lift $ lift $ Pg.query_ c "select * from coffee_logs"
      mapM_ S.yield $ fmap lValue rows
      release key

  writer ev = lift $ Pool.withResource pool $ \c -> do
    _ <- Pg.execute c "insert into coffee_logs (value) values (?);" $ Pg.Only (Pg.toJSONField ev)
    pure ()

  setupDb c = do
    Pg.withTransaction c $ do
      _ <- Pg.query c "select pg_advisory_xact_lock(?);" (Pg.Only tableHash) :: IO [Pg.Only ()]
      void $ Pg.execute_ c "CREATE TABLE IF NOT EXISTS coffee_logs (id serial primary key, value jsonb);"

  borrowConn :: RIO (ReleaseKey, Pg.Connection)
  borrowConn = do
    (key, (conn, _)) <- allocate (Pool.takeResource pool) (\(conn, lpool) -> Pool.putResource lpool conn)
    return (key, conn)

  tableHash = hash ("coffee_logs" :: String)

dropPgEvents :: Pool Pg.Connection -> IO ()
dropPgEvents pool = do
  Pool.withResource pool $ \c -> do
    void $ Pg.execute_ c "DROP TABLE IF EXISTS coffee_logs;"

runPg :: RIO a -> IO a
runPg = runResourceT
