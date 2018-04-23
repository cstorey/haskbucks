{-# LANGUAGE FlexibleContexts #-}

module HaskbucksSpec (spec) where

import           Test.Hspec
import           Haskbucks
import qualified Control.Monad.State.Strict as State
import Control.Arrow ((***))

spec :: Spec
spec = parallel $ do
  describe "PureCashier" $ do
    it "Records that an order has occurred" $ do
      let (_, logs) = run pureCashier [] coOrder
      
      logs `shouldContain` [OrderedCoffee]

    it "Disallows taking by default" $ do
      let (res, _) = run pureCashier [] $
                      \cashier -> do
                              order <- coOrder cashier
                              coTake cashier order
      
      res `shouldBe` Left NotReady
    it "Returns coffee when prepared" $ do
      let (order, hist) = run pureCashier [] $ coOrder
      let ((), hist') = run pureBarista hist $ bPrepareDrink
      let (coffee, _) = run pureCashier hist' $ flip coTake order
      
      coffee `shouldBe` Right ACoffee

    it "Cant take coffee twice" $ do
      let (order, hist) = run pureCashier [] $ coOrder
      let ((), hist') = run pureBarista hist $ bPrepareDrink
      let (_, hist'') = run pureCashier hist' $ flip coTake order
      let (coffee, _) = run pureCashier hist'' $ flip coTake order
      
      coffee `shouldBe` Left AlreadyTaken

  where
  run ops hist action = (id *** (hist++)) $ State.evalState (action ops) hist $ ()
