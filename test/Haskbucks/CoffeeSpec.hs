{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskbucks.CoffeeSpec (spec) where

import           Test.Hspec
import           Haskbucks
import           Haskbucks.Junk
import qualified Control.Concurrent.STM as STM
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

spec :: Spec
spec = do
  describe "Coffee" $ do
    describe "Pure" $ around withStateEvents $ cashierContract runState
    describe "STM" $ around withStmEvents $ cashierContract runStm
    describe "Pg" $ around withPgFromEnv $ cashierContract runPg
  describe "Smoke" $ do
    describe "STM" $ around withStmEvents $ smokeTest STM.retry runStm
