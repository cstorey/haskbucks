{-# LANGUAGE FlexibleContexts #-}
module Haskbucks where

import Data.IORef
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class

import GHC.Stack

import Data.Foldable (foldl')

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype OrderId = OrderId ()
    deriving (Show, Ord, Eq)

data Cup = ACoffee
  deriving (Show, Eq)

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

pureCashier :: (HasCallStack, MonadReader [OrderEvent] m, MonadWriter [OrderEvent] m) => CustomerOps m
pureCashier = CustomerOps takeOrder takeCoffee
  where
    takeOrder = do
      tell [OrderedCoffee]
      return $ OrderId ()
    takeCoffee _ = do
      st <- evalOrderHistory <$> ask
      case st of
        OrderReady -> do
          tell [OrderDelivered]
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


pureBarista :: (HasCallStack, MonadReader [OrderEvent] m, MonadWriter [OrderEvent] m) => BaristaOps m
pureBarista = BaristaOps prepareDrink
  where
    prepareDrink = do
      st <- evalOrderHistory <$> ask
      case st of
        OrderAccepted -> do
          tell [OrderPrepared]
        _ -> pure ()

     
