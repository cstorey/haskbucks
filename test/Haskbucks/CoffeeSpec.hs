{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskbucks.CoffeeSpec (spec) where

import           Test.Hspec
import           Haskbucks
import           Haskbucks.Junk
import qualified Data.Map.Strict as Map
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import System.Timeout
-- import Debug.Trace

cashierContract :: Monad m => (forall a . m a -> IO a) -> SpecWith (EventLog (OrderId, OrderEvent) m)
cashierContract run = do
  it "Records that an order has occurred" $ \events -> do
    (o, logs) <- run $ do
          let cashier = pureCashier events
          o <- coOrder cashier
          h <- snapshot events
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


smokeTest ::  forall m . Monad m => m () -> (forall a . m a -> IO a) -> SpecWith (EventLog (OrderId, OrderEvent) m)
smokeTest pause run = do
  xit "should prepare drinks asynchronously" $ \events -> do
    res <- withAsync (run $ runABarista events) $ \a -> do
      link a
      timeout 1000 $ run $ runCashier events
    res `shouldBe` Just (Right ACoffee)

  where
  runCashier :: EventLog (OrderId, OrderEvent) m -> m (Either CoffeeError Cup)
  runCashier events = do
    let cashier = pureCashier events
    order <- coOrder cashier
    do
      r <- coTake cashier order
      case r of
        Left NotReady ->  pause >> runCashier events
        other -> return other

  runABarista :: EventLog (OrderId, OrderEvent) m -> m ()
  runABarista events = do
    let barista = pureBarista events
    st <- evalOrderHistory <$> snapshot events
    let toServe = Map.filter (\case OrderAccepted -> True; _ -> False) st
    if Map.null toServe
    then pause >> runABarista events
    else forM_ (Map.toList toServe) $ \(orderId, _) -> do
        bPrepareDrink barista orderId

spec :: Spec
spec = do
  describe "Coffee" $ do
    describe "Pure" $ around withStateEvents $ cashierContract runState
    describe "STM" $ around withStmEvents $ cashierContract runStm
    describe "Pg" $ around withPgFromEnv $ cashierContract runPg
  describe "Smoke" $ do
    describe "STM" $ around withStmEvents $ smokeTest (threadDelay 100) runStm
