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

class Monad m => (MonadEvents e) m where
  history :: m [e]
  append :: e -> m ()

data CoffeeError =
    NotReady
  | AlreadyTaken
  deriving (Show, Eq)

data CustomerOps f = CustomerOps {
    coOrder :: f OrderId
,   coTake :: OrderId -> f (Either CoffeeError Cup)
}

data BaristaOps f = BaristaOps {
  bPrepareDrink :: f ()
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

pureCashier :: (HasCallStack, MonadEvents OrderEvent m) => CustomerOps m
pureCashier = CustomerOps takeOrder takeCoffee
  where
    takeOrder = do
      append OrderedCoffee
      return $ OrderId ()
    takeCoffee _ = do
      st <- evalOrderHistory <$> history
      case st of
        OrderReady -> do
          append OrderDelivered
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


pureBarista :: (HasCallStack, MonadEvents OrderEvent m) => BaristaOps m
pureBarista = BaristaOps prepareDrink
  where
    prepareDrink = do
      st <- evalOrderHistory <$> history
      case st of
        OrderAccepted -> do
          append OrderPrepared
        _ -> pure ()

     
