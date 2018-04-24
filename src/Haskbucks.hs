{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Haskbucks where

import Data.IORef

import GHC.Stack

import Data.Foldable (foldl')

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype OrderId = OrderId ()
    deriving (Show, Ord, Eq)

data Cup = ACoffee
  deriving (Show, Eq)

data EventLog ev m = EventLog {
  history :: m [ev]
, append :: ev -> m ()
}

data CoffeeError =
    NotReady
  | AlreadyTaken
  deriving (Show, Eq)

data CustomerOps f = CustomerOps {
    coOrder :: f OrderId
,   coTake :: OrderId -> f (Either CoffeeError Cup)
}

data BaristaOps f = BaristaOps {
  bPrepareDrink :: OrderId -> f ()
}

data OrderEvent =
    OrderedCoffee
  | OrderPrepared
  | OrderDelivered
  deriving (Show, Eq)

newCashier :: IO (CustomerOps IO)
newCashier = do
    ids <- newIORef ()
    return $ CustomerOps (doOrder ids) (doTake ids)

    where
        doOrder ids = OrderId <$> atomicModifyIORef ids (\i -> (succ i, succ i))
        doTake _ _id = return $ Right ACoffee

data OrderState =
    OrderStart
  | OrderAccepted
  | OrderReady
  | OrderCompleted
  deriving (Show)

pureCashier :: (HasCallStack, Monad m) => EventLog OrderEvent m -> CustomerOps m
pureCashier events = CustomerOps takeOrder takeCoffee
  where
    takeOrder = do
      append events OrderedCoffee
      return $ OrderId ()
    takeCoffee _ = do
      st <- evalOrderHistory <$> history events
      case st of
        OrderReady -> do
          append events OrderDelivered
          return $ Right ACoffee
        OrderCompleted -> return $ Left AlreadyTaken
        _ -> return $ Left NotReady

evalOrderHistory :: [OrderEvent] -> OrderState
evalOrderHistory = foldl' applyOrderEvent OrderStart
  where
  applyOrderEvent :: OrderState -> OrderEvent -> OrderState
  applyOrderEvent OrderStart OrderedCoffee = OrderAccepted
  applyOrderEvent OrderAccepted OrderPrepared = OrderReady
  applyOrderEvent OrderReady OrderDelivered = OrderCompleted
  applyOrderEvent st _ev = st


pureBarista :: (HasCallStack, Monad m) => EventLog OrderEvent m -> BaristaOps m
pureBarista events = BaristaOps prepareDrink
  where
    prepareDrink _order = do
      st <- evalOrderHistory <$> history events
      case st of
        OrderAccepted -> do
          append events OrderPrepared
        _ -> pure ()
