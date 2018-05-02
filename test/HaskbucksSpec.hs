{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskbucksSpec (spec) where

import           Test.Hspec
import           Haskbucks
import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (MonadState, State)
import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Data.Aeson as JSON
import           Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified System.Posix.Env.ByteString as Env
import           Data.Typeable (Typeable)
import Data.Hashable (hash)

import Control.Exception (bracket)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import Control.Concurrent.Async
import Control.Monad
import System.Timeout
-- import Debug.Trace

cashierContract :: Monad m => (forall a . m a -> IO a) -> SpecWith (EventLog (OrderId, OrderEvent) m)
cashierContract run = do
  it "Records that an order has occurred" $ \events -> do
    (o, logs) <- run $ do
          let cashier = pureCashier events
          o <- coOrder cashier
          h <- history events
          return (o, h)

    logs `shouldContain` [(o, OrderedCoffee)]

  it "Disallows taking by default" $ \events -> do
    res <- run $ do
          let cashier = pureCashier events
          order <- coOrder cashier
          coTake cashier order

    res `shouldBe` Left NotReady
  it "Returns coffee when prepared" $ \events -> do
    coffee <- run $ do
          let cashier = pureCashier events
          let barista = pureBarista events
          order <- coOrder cashier
          bPrepareDrink barista order
          coTake cashier order

    coffee `shouldBe` Right ACoffee

  it "Cant take coffee twice" $ \events -> do
    coffee <- run $ do
          let cashier = pureCashier events
          let barista = pureBarista events
          order <- coOrder cashier
          bPrepareDrink barista order
          _ <- coTake cashier order
          coTake cashier order

    coffee `shouldBe` Left AlreadyTaken

  it "Can order multiple drinks" $ \events -> do
    coffees <- run $ do
          let cashier = pureCashier events
          let barista = pureBarista events
          o0 <- coOrder cashier
          bPrepareDrink barista o0
          c0 <- coTake cashier o0
          o1 <- coOrder cashier
          bPrepareDrink barista o1
          c1 <- coTake cashier o1
          return $ sequence [c0, c1]

    coffees `shouldBe` Right [ACoffee, ACoffee]


logContract :: Monad m => (forall a . m a -> IO a) -> SpecWith (EventLog String m)
logContract run = do
  it "Starts off empty" $ \events -> do
    ev <- run $ history events
    ev `shouldBe` []
  it "Adding an item" $ \events -> do
    ev <- run $ do
      append events "a"
      history events
    ev `shouldBe` ["a"]
  it "Adding two items" $ \events -> do
    ev <- run $ do
      append events "a"
      append events "b"
      history events
    ev `shouldBe` ["a", "b"]


smokeTest ::  Monad m => (forall z . m z) -> (forall a . m a -> IO a) -> SpecWith (EventLog (OrderId, OrderEvent) m)
smokeTest retry run = do
  it "should prepare drinks asynchronously" $ \events -> do
    res <- withAsync (run $ runABarista events) $ \a -> do
      link a
      let cashier = pureCashier events
      timeout 1000 $ do
        order <- run $ coOrder cashier
        run $ do
          r <- coTake cashier order
          case r of
            Left NotReady -> retry
            _ -> return r

    res `shouldBe` Just (Right ACoffee)

  where
  runABarista events = do
    let barista = pureBarista events
    st <- evalOrderHistory <$> history events
    let toServe = Map.filter (\case OrderAccepted -> True; _ -> False) st
    if Map.null toServe
    then retry
    else forM_ (Map.toList toServe) $ \(orderId, _) -> do
        bPrepareDrink barista orderId

runState :: State [ev] a -> IO a
runState = pure . flip State.evalState []

withStateEvents :: (EventLog ev (State [ev]) -> IO a) -> IO a
withStateEvents f = do
  f stateLogger
  where
  stateLogger :: MonadState [a] m => EventLog a m
  stateLogger = EventLog getter writer
    where
    getter = State.get
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

    let getter = STM.readTVar evVar
    let writer ev = STM.modifyTVar' evVar (++ [ev])

    pure $ EventLog getter writer


data PgLogItem ev = PgLogItem {
  _lId :: Integer,
  lValue :: ev
} deriving (Show, Typeable)

instance (Typeable ev, JSON.FromJSON ev) => Pg.FromRow (PgLogItem ev) where
  fromRow = PgLogItem <$> Pg.field <*> Pg.fieldWith Pg.fromJSONField

withPgEvents :: (Typeable ev, JSON.ToJSON ev, JSON.FromJSON ev) => (EventLog ev IO -> IO ()) -> IO ()
withPgEvents f = do
  pgUrl <- Env.getEnv $  Char8.pack "PG_URL"
  case pgUrl of
    Nothing -> do pendingWith "No $PG_URL specified"
    Just url -> do
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
  getter = Pool.withResource pool $ \c -> do
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

spec :: Spec
spec = do
  describe "EventLog" $ do
    describe "Pure" $ around withStateEvents $ logContract runState
    describe "STM" $ around withStmEvents $ logContract runStm
    describe "Pg" $ around withPgEvents $ logContract runPg
  describe "Coffee" $ do
    describe "Pure" $ around withStateEvents $ cashierContract runState
    describe "STM" $ around withStmEvents $ cashierContract runStm
    describe "Pg" $ around withPgEvents $ cashierContract runPg
  describe "Smoke" $ do
    describe "STM" $ around withStmEvents $ smokeTest STM.retry runStm
