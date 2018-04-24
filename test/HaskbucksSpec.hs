{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module HaskbucksSpec (spec) where

import           Test.Hspec
import           Haskbucks
import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (MonadState, State)

spec :: Spec
spec = parallel $ do
  describe "Pure" $ cashierContract stateLogger runState
  where
  cashierContract :: Monad m => EventLog OrderEvent m -> (forall a . m a -> IO (a, [OrderEvent])) -> Spec
  cashierContract events run = do
    let cashier = pureCashier events
    let barista = pureBarista events

    it "Records that an order has occurred" $ do
      (_, logs) <- run $ do
           coOrder cashier

      logs `shouldContain` [OrderedCoffee]

    it "Disallows taking by default" $ do
      (res, _) <- run $ do
            order <- coOrder cashier
            coTake cashier order

      res `shouldBe` Left NotReady
    it "Returns coffee when prepared" $ do
      (coffee, _) <- run $ do
            order <- coOrder cashier
            bPrepareDrink barista order
            coTake cashier order

      coffee `shouldBe` Right ACoffee

    it "Cant take coffee twice" $ do
      (coffee, _) <- run $ do
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
