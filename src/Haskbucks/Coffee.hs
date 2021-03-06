{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Haskbucks.Coffee where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as JSON
import Haskbucks.Event

import GHC.Stack
import GHC.Generics

import Data.Foldable (foldl')

newtype OrderId = OrderId Int
    deriving (Show, Ord, Eq, Enum, Generic)

instance JSON.FromJSON OrderId
instance JSON.ToJSON OrderId

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
  bPrepareDrink :: OrderId -> f ()
}

data OrderEvent =
    OrderedCoffee
  | OrderPrepared
  | OrderDelivered
  deriving (Show, Eq, Generic)
instance JSON.FromJSON OrderEvent
instance JSON.ToJSON OrderEvent

data OrderState =
    OrderStart
  | OrderAccepted
  | OrderReady
  | OrderCompleted
  deriving (Show)

pureCashier :: forall m . (HasCallStack, Monad m) => EventLog (OrderId, OrderEvent) m -> CustomerOps m
pureCashier events = CustomerOps takeOrder takeCoffee
  where
    takeOrder = do
      newId <- maybe (OrderId 0) (succ . fst) . Map.lookupMax . evalOrderHistory <$> snapshot events
      append events (newId, OrderedCoffee)
      return newId
    takeCoffee orderId = do
      st <- evalOrderHistory <$> snapshot events
      case Map.lookup orderId st of
        Just OrderReady -> do
          append events (orderId, OrderDelivered)
          return $ Right ACoffee
        Just OrderCompleted -> return $ Left AlreadyTaken
        _ -> return $ Left NotReady

evalOrderHistory :: [(OrderId, OrderEvent)] -> Map OrderId OrderState
evalOrderHistory = foldl' apply Map.empty
  where
  apply :: Map OrderId OrderState -> (OrderId, OrderEvent) -> Map OrderId OrderState
  apply m (oid, ev) =
    let st = maybe OrderStart id $ Map.lookup oid m
        st' = applyOrderEvent st ev
    in
    Map.insert oid st' m


  applyOrderEvent :: OrderState -> OrderEvent -> OrderState
  applyOrderEvent OrderStart OrderedCoffee = OrderAccepted
  applyOrderEvent OrderAccepted OrderPrepared = OrderReady
  applyOrderEvent OrderReady OrderDelivered = OrderCompleted
  applyOrderEvent st _ev = st


pureBarista :: (HasCallStack, Monad m) => EventLog (OrderId, OrderEvent) m -> BaristaOps m
pureBarista events = BaristaOps prepareDrink
  where
    prepareDrink order = do
      st <- evalOrderHistory <$> snapshot events
      case Map.lookup order st  of
        Just OrderAccepted -> do
          append events (order, OrderPrepared)
        _ -> pure ()
