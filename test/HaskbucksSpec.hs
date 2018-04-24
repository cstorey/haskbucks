{-# LANGUAGE FlexibleContexts #-}

module HaskbucksSpec (spec) where

import           Test.Hspec
import           Haskbucks
import qualified Control.Monad.State.Strict as State
import           Control.Monad.State.Strict (MonadState, State)

spec :: Spec
spec = parallel $ do
  describe "PureCashier" $ do
    let events = stateLogger
    let cashier = pureCashier events
    let barista = pureBarista events

    it "Records that an order has occurred" $ do
      let (_, logs) = run $ do
           coOrder cashier

      logs `shouldContain` [OrderedCoffee]

    it "Disallows taking by default" $ do
      let (res, _) = run $ do
            order <- coOrder cashier
            coTake cashier order

      res `shouldBe` Left NotReady
    it "Returns coffee when prepared" $ do
      let (coffee, _) = run $ do
            order <- coOrder cashier
            bPrepareDrink barista order
            coTake cashier order

      coffee `shouldBe` Right ACoffee

    it "Cant take coffee twice" $ do
      let (coffee, _) = run $ do
            order <- coOrder cashier
            bPrepareDrink barista order
            _ <- coTake cashier order
            coTake cashier order

      coffee `shouldBe` Left AlreadyTaken

  where
  run :: State [ev] a -> (a, [ev])
  run = flip State.runState []
  stateLogger :: MonadState [a] m => EventLog a m
  stateLogger = EventLog getter writer
    where
    getter = State.get
    writer ev = State.modify $ (++ [ev])
