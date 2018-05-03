{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskbucks.EventSpec (spec) where

import           Test.Hspec
import           Haskbucks.Event
import           Haskbucks.Junk
import qualified Streaming.Prelude as S

-- import Debug.Trace

logContract :: Monad m => (forall a . m a -> IO a) -> SpecWith (EventLog String m)
logContract run = do
  it "Starts off empty" $ \events -> do
    ev <- run $ snapshot events
    ev `shouldBe` []
  it "Adding an item" $ \events -> do
    ev <- run $ do
      append events "a"
      snapshot events
    ev `shouldBe` ["a"]
  it "Adding two items" $ \events -> do
    ev <- run $ do
      append events "a"
      append events "b"
      snapshot events
    ev `shouldBe` ["a", "b"]

  it "Stream returns same as snapshot" $ \events -> do
    (h, s) <- run $ do
      append events "a"
      append events "b"
      h <- snapshot events
      s <- S.toList_ $ S.take 2 $ stream events 
      pure (h, s)
    s `shouldBe` h


spec :: Spec
spec = parallel $ do
  describe "EventLog" $ do
    describe "Pure" $ around withStateEvents $ logContract runState
    describe "STM" $ around withStmEvents $ logContract runStm
    describe "Pg" $ around withPgFromEnv $ logContract runPg
