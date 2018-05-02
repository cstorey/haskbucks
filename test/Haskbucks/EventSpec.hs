{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskbucks.EventSpec (spec) where

import           Test.Hspec
import           Haskbucks.Event
import           Haskbucks.Junk
import           Data.Monoid

logContract :: Monad m => (forall a . m a -> IO a) -> SpecWith (EventLog String m)
logContract run = do
  it "Starts off empty" $ \events -> do
    ev <- run $ history events
    ev `shouldBe` []
  it "Adding an item" $ \events -> do
    ev <- run $ do
      append events "a"
      history events
    ev `shouldBe` ["a"]
  it "Adding two items" $ \events -> do
    ev <- run $ do
      append events "a"
      append events "b"
      history events
    ev `shouldBe` ["a", "b"]

  it "Adding two items" $ \events -> do
    ev <- run $ do
      append events "a"
      append events "b"
      history events
    ev `shouldBe` ["a", "b"]

  it "Should allow pulling events since 'X'" $ \events -> do
    evs <- run $ do
      append events "a"
      maxId <- getLast <$> foldMap (Last . Just . fst) <$> historySince events Nothing
      append events "b"
      evs <- historySince events $ maxId
      pure $ fmap snd evs
    evs `shouldBe` ["b"]

  it "Should allow pulling events since max of empty history" $ \events -> do
    evs <- run $ do
      maxId <- getLast <$> foldMap (Last . Just . fst) <$> historySince events Nothing
      append events "b"
      evs <- historySince events $ maxId
      pure $ fmap snd evs
    evs `shouldBe` ["b"]



spec :: Spec
spec = do
  describe "EventLog" $ do
    describe "Pure" $ around withStateEvents $ logContract runState
    describe "STM" $ around withStmEvents $ logContract runStm
    describe "Pg" $ around withPgFromEnv $ logContract runPg
