{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskbucks.Event
( EventLog(..)
, history
, runState
, withStateEvents
, runStm
, withStmEvents
, runPg
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

newtype Stamp = Stamp { unStamp :: Int }
  deriving (Show, Ord, Eq, Bounded)

data EventLog ev m = EventLog {
  historySince :: Maybe Stamp -> m [(Stamp, ev)]
, append :: ev -> m ()
}

history :: Monad m => EventLog ev m -> m [ev]
history evs = fmap snd <$> historySince evs Nothing

data PgLogItem ev = PgLogItem {
  lId :: Int,
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
  stateLogger :: EventLog a (State [a])
  stateLogger = EventLog getter writer
    where
    getter since = do
      evs <- zip (map Stamp [0..]) <$> State.get
      pure $ drop (maybe 0 (succ . unStamp) since) evs

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

    let getter since = do
                        evs <- zip (map Stamp [0..]) <$> STM.readTVar evVar
                        pure $ drop (maybe 0 (succ . unStamp) since) evs
    let writer ev = STM.modifyTVar' evVar (++ [ev])

    pure $ EventLog getter writer


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
  pure $ EventLog getter writer
  where
  getter since = Pool.withResource pool $ \c -> do
      let sid = fmap unStamp since
      rows <- Pg.query c "select * from coffee_logs where ? is null or id > ?" (sid, sid)
      pure $ fmap (\r -> (Stamp $ lId r, lValue r)) rows

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
