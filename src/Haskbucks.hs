module Haskbucks where

import Data.IORef

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype OrderId = OrderId (Int)
    deriving (Show, Ord, Eq)

data Cup = ACoffee

data CustomerOrder f = CustomerOrder {
    orderCoffee :: f OrderId
,   takeCoffee :: OrderId -> f Cup
}

newCashier :: IO (CustomerOrder IO)
newCashier = do
    ids <- newIORef 0
    return $ CustomerOrder (doOrder ids) (doTake ids)

    where
        doOrder ids = OrderId <$> atomicModifyIORef ids (\i -> (succ i, succ i))
        doTake _ _id = return ACoffee