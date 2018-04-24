{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module HaskbucksSpec (spec) where

import           Test.Hspec
import           Haskbucks
import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (MonadState, State)

spec :: Spec
spec = parallel $ do
  describe "Pure" $ around withStateEvents $ cashierContract runState
  where
  cashierContract :: Monad m => (forall a . m a -> IO (a, [OrderEvent])) -> SpecWith (EventLog OrderEvent m)
  cashierContract run = do
    it "Records that an order has occurred" $ \events -> do
      (_, logs) <- run $ do
           let cashier = pureCashier events
           coOrder cashier

      logs `shouldContain` [OrderedCoffee]

    it "Disallows taking by default" $ \events -> do
      (res, _) <- run $ do
            let cashier = pureCashier events
            order <- coOrder cashier
            coTake cashier order

      res `shouldBe` Left NotReady
    it "Returns coffee when prepared" $ \events -> do
      (coffee, _) <- run $ do
            let cashier = pureCashier events
            let barista = pureBarista events
            order <- coOrder cashier
            bPrepareDrink barista order
            coTake cashier order

      coffee `shouldBe` Right ACoffee

    it "Cant take coffee twice" $ \events -> do
      (coffee, _) <- run $ do
            let cashier = pureCashier events
            let barista = pureBarista events
            order <- coOrder cashier
            bPrepareDrink barista order
            _ <- coTake cashier order
            coTake cashier order

      coffee `shouldBe` Left AlreadyTaken

  runState :: State [ev] a -> IO (a, [ev])
  runState = pure . flip State.runState []
  stateLogger :: MonadState [a] m => EventLog a m
  stateLogger = EventLog getter writer
    where
    getter = State.get
    writer ev = State.modify $ (++ [ev])

  withStateEvents :: (EventLog OrderEvent (State [OrderEvent]) -> IO a) -> IO a
  withStateEvents f = do
    f stateLogger
