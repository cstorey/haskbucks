{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module HaskbucksSpec (spec) where

import           Test.Hspec
import           Haskbucks
import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (MonadState, State)
import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM


cashierContract :: Monad m => (forall a . m a -> IO a) -> SpecWith (EventLog OrderEvent m)
cashierContract run = do
  it "Records that an order has occurred" $ \events -> do
    logs <- run $ do
         let cashier = pureCashier events
         coOrder cashier
         history events

    logs `shouldContain` [OrderedCoffee]

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

runState :: State [ev] a -> IO a
runState = pure . flip State.evalState []
withStateEvents :: (EventLog OrderEvent (State [OrderEvent]) -> IO a) -> IO a
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
  describe "Pure" $ around withStateEvents $ cashierContract runState
  describe "STM" $ around withStmEvents $ cashierContract runStm
