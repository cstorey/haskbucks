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
import Control.Concurrent (threadDelay)


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
  lId :: Integer,
  lValue :: ev
} deriving (Show, Typeable)

instance (Typeable ev, JSON.FromJSON ev) => Pg.FromRow (PgLogItem ev) where
  fromRow = PgLogItem <$> Pg.field <*> Pg.fieldWith Pg.fromJSONField

-- Backoff time
pgRetryTime :: Int
pgRetryTime = 100 * 1000

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

runStm :: IO a -> IO a
runStm = id

withStmEvents :: forall ev a . (EventLog ev IO -> IO a) -> IO a
withStmEvents f = do
  events <- newStmEvents
  f events
  where
  newStmEvents :: IO (EventLog ev IO)
  newStmEvents = do
    evVar <- STM.atomically $ STM.newTVar []

    let getSnapshot = STM.readTVarIO evVar
    let consumer n = do
                  -- liftIO $ putStrLn $ show ("stm start offset"::String, n)
                  evs <- lift $ STM.atomically $ do
                    evs <- drop n <$> STM.readTVar evVar
                    if length evs > 0
                    then pure evs
                    else STM.retry
                  mapM_ S.yield evs
                  let off = n + length evs 
                  -- liftIO $ putStrLn $ show ("stm consumed offset"::String, off)
                  consumer off

    let writer ev = STM.atomically $ STM.modifyTVar' evVar (++ [ev])

    pure $ EventLog getSnapshot writer (consumer 0)


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

  consumer = go Nothing
    where
    go :: Maybe Integer -> Stream (Of ev) (ResourceT IO) b1
    go off = do
      -- liftIO $ putStrLn $ show ("pg start offset"::String, off)
      evs <- lift $ do
        (key, c) <- borrowConn
        rows <- liftIO $ Pg.query c "select * from coffee_logs where (id > ? or ? is null)" $ (off, off)
        release key
        pure rows
      if length evs > 0
      then mapM_ S.yield $ fmap lValue evs
      else liftIO $ threadDelay pgRetryTime
      let off' = foldr max off $ fmap (Just . lId) evs
      -- liftIO $ putStrLn $ show ("pg consume offset"::String, off')
      go $ off'

  writer ev = lift $ Pool.withResource pool $ \c -> do
    res <- Pg.query c "insert into coffee_logs (value) values (?) returning (id);" $ Pg.Only (Pg.toJSONField ev)
    let _ = res :: [Pg.Only Int]
    -- liftIO $ putStrLn $ show ("insert at "::String, res)
    pure ()

  setupDb c = do
    Pg.withTransaction c $ do
      _ <- Pg.query c "select pg_advisory_xact_lock(?);" (Pg.Only tableHash) :: IO [Pg.Only ()]
      void $ Pg.execute_ c "DROP TABLE IF EXISTS coffee_logs ;"
      void $ Pg.execute_ c "CREATE TABLE IF NOT EXISTS coffee_logs (id serial primary key, value jsonb);"

  borrowConn :: RIO (ReleaseKey, Pg.Connection)
  borrowConn = poolAlloc pool

  poolAlloc :: MonadResource m => Pool a -> m (ReleaseKey, a)
  poolAlloc pool = do
    (key, (res, _lpool)) <- allocate
      (Pool.takeResource pool)
      (\(conn, lpool) -> Pool.putResource lpool conn)
    return (key, res)

  tableHash = hash ("coffee_logs" :: String)

dropPgEvents :: Pool Pg.Connection -> IO ()
dropPgEvents pool = do
  Pool.withResource pool $ \c -> do
    void $ Pg.execute_ c cleanup
  where
  cleanup = "DO $$\n\
            \  DECLARE r record;\n\
            \BEGIN\n\
            \  FOR r IN SELECT table_schema, table_name FROM information_schema.tables\n\
            \    WHERE table_type = 'BASE TABLE' AND table_schema = 'public' AND table_name = 'coffee_logs'\n\
            \  LOOP\n\
            \    EXECUTE 'TRUNCATE TABLE ' || quote_ident(r.table_schema) || '.' || quote_ident(r.table_name);\n\
            \  END LOOP;\n\
            \END\n$$;"

runPg :: RIO a -> IO a
runPg = runResourceT
