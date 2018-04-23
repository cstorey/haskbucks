{-# LANGUAGE FlexibleContexts #-}
module Haskbucks where

import Data.IORef
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class

import GHC.Stack

import Data.Foldable (foldl')

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype OrderId = OrderId (Int)
    deriving (Show, Ord, Eq)

data Cup = ACoffee
  deriving (Show, Eq)

data CoffeeError =
    NotReady
  | AlreadyTaken
  deriving (Show, Eq)

data CustomerOrder f = CustomerOrder {
    coOrder :: f OrderId
,   coTake :: OrderId -> f (Either CoffeeError Cup)
}


data OrderEvent =
    OrderedCoffee
  | OrderPrepared
  | OrderDelivered
  deriving (Show, Eq)

data CashierState = CashierState {
  prevOrderId :: Int
}

newCashier :: IO (CustomerOrder IO)
newCashier = do
    ids <- newIORef 0
    return $ CustomerOrder (doOrder ids) (doTake ids)

    where
        doOrder ids = OrderId <$> atomicModifyIORef ids (\i -> (succ i, succ i))
        doTake _ _id = return $ Right ACoffee

data OrderState = 
    OrderStart
  | OrderAccepted
  | OrderReady
  | OrderCompleted
  deriving (Show)

pureCashier :: (HasCallStack, MonadReader [OrderEvent] m, MonadState CashierState m, MonadWriter [OrderEvent] m) => CustomerOrder m
pureCashier = CustomerOrder takeOrder takeCoffee
  where
    takeOrder = do
      order <- OrderId <$> gets prevOrderId
      modify $ \st -> st { prevOrderId = succ $ prevOrderId st }
      tell [OrderedCoffee]
      return order
    takeCoffee _ = do
      st <- evalHistory <$> ask
      case st of
        OrderReady -> do
          tell [OrderDelivered]
          return $ Right ACoffee
        OrderCompleted -> return $ Left AlreadyTaken
        _ -> return $ Left NotReady

    evalHistory :: [OrderEvent] -> OrderState
    evalHistory = foldl' applyEvent OrderStart 
    applyEvent :: OrderState -> OrderEvent -> OrderState
    applyEvent OrderStart OrderedCoffee = OrderAccepted
    applyEvent OrderAccepted OrderPrepared = OrderReady
    applyEvent OrderReady OrderDelivered = OrderCompleted
    applyEvent st _ev = st -- error $ "applyEvent: " ++ show (st, ev)
