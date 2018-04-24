{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module HaskbucksSpec (spec) where

import           Test.Hspec
import           Haskbucks
import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (MonadState, State)
import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM

spec :: Spec
spec = parallel $ do
  describe "Pure" $ around withStateEvents $ cashierContract runState
  describe "STM" $ around withStmEvents $ cashierContract runStm
  where
  cashierContract :: Monad m => (forall a . EventLog OrderEvent m -> m a -> IO (a, [OrderEvent])) -> SpecWith (EventLog OrderEvent m)
  cashierContract run = do
    it "Records that an order has occurred" $ \events -> do
      (_, logs) <- run events $ do
           let cashier = pureCashier events
           coOrder cashier

      logs `shouldContain` [OrderedCoffee]

    it "Disallows taking by default" $ \events -> do
      (res, _) <- run events $ do
            let cashier = pureCashier events
            order <- coOrder cashier
            coTake cashier order

      res `shouldBe` Left NotReady
    it "Returns coffee when prepared" $ \events -> do
      (coffee, _) <- run events $ do
            let cashier = pureCashier events
            let barista = pureBarista events
            order <- coOrder cashier
            bPrepareDrink barista order
            coTake cashier order

      coffee `shouldBe` Right ACoffee

    it "Cant take coffee twice" $ \events -> do
      (coffee, _) <- run events $ do
            let cashier = pureCashier events
            let barista = pureBarista events
            order <- coOrder cashier
            bPrepareDrink barista order
            _ <- coTake cashier order
            coTake cashier order

      coffee `shouldBe` Left AlreadyTaken

  runState :: EventLog ev (State [ev]) -> State [ev] a -> IO (a, [ev])
  runState _ = pure . flip State.runState []
  stateLogger :: MonadState [a] m => EventLog a m
  stateLogger = EventLog getter writer
    where
    getter = State.get
    writer ev = State.modify $ (++ [ev])

  withStateEvents :: (EventLog OrderEvent (State [OrderEvent]) -> IO a) -> IO a
  withStateEvents f = do
    f stateLogger

  runStm :: EventLog OrderEvent STM -> STM a -> IO (a, [OrderEvent])
  runStm events action = do
    r <- STM.atomically action
    logs <- STM.atomically $ history events
    pure (r, logs)

  withStmEvents :: (EventLog ev STM -> IO a) -> IO a
  withStmEvents f = do
    events <- STM.atomically $ newStmEvents
    f events

  newStmEvents :: STM (EventLog a STM)
  newStmEvents = do
    evVar <- STM.newTVar []
    
    let getter = STM.readTVar evVar
    let writer ev = STM.modifyTVar' evVar (++ [ev])

    pure $ EventLog getter writer
