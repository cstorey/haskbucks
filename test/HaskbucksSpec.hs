{-# LANGUAGE FlexibleContexts #-}

module HaskbucksSpec (spec) where

import           Test.Hspec
import           Haskbucks
import qualified Control.Monad.RWS.Strict as RWS
import Control.Arrow ((***))

spec :: Spec
spec = parallel $ do
  describe "PureCashier" $ do
    it "Records that an order has occurred" $ do
      let (_, logs) = runCashierPure [] coOrder
      
      logs `shouldContain` [OrderedCoffee]

    it "Disallows taking by default" $ do
      let (res, _) = runCashierPure [] $
                      \cashier -> do
                              order <- coOrder cashier
                              coTake cashier order
      
      res `shouldBe` Left NotReady
    it "Returns coffee when prepared" $ do
      let (order, hist) = runCashierPure [] $ coOrder
      let (coffee, _) = runCashierPure (hist ++ [OrderPrepared]) $ flip coTake order
      
      coffee `shouldBe` Right ACoffee

    it "Cant take coffee twice" $ do
      let (order, hist) = runCashierPure [] $ coOrder
      let (_, hist') = runCashierPure (hist ++ [OrderPrepared]) $ flip coTake order
      let (coffee, _) = runCashierPure hist' $ flip coTake order
      
      coffee `shouldBe` Left AlreadyTaken

  where
  runCashierPure history action = (id *** (history++)) $ RWS.evalRWS (action pureCashier) history ()
