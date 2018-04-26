{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module HaskbucksSpec (spec) where

import           Test.Hspec
import           Haskbucks
import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (MonadState, State)
import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async
import System.Timeout


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


-- Need to full out log tailer / process manager plumbing goop for this to work.

-- observe: State -> Event -> State (left fold)
-- Observe needs to
-- execute: Monad m => SomeCommandOps m -> EventLog ev m -> State -> m a

-- Process manager needs to record private events / state, too. Might end up
-- needing polymorphic observe (or just observe over sums)

smokeTest :: Spec
smokeTest = do
  describe "Async baristas" $ around withStmEvents $ do
    it "should ... somethijng?" $ \events -> do
      withAsync runABarista $ \a -> do
        link a
        let cashier = pureCashier events
        order <- runStm $ coOrder cashier
        timeout 1000 $ do


      False `shouldBe` True

  where
  runABarista = error "barista...?"


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

spec :: Spec
spec = parallel $ do
  describe "EventLog" $ do
    describe "Pure" $ around withStateEvents $ logContract runState
    describe "STM" $ around withStmEvents $ logContract runStm
  describe "Coffee" $ do
    describe "Pure" $ around withStateEvents $ cashierContract runState
    describe "STM" $ around withStmEvents $ cashierContract runStm
  describe "Smoke" $ do
    smokeTest
