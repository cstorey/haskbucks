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
import qualified Control.Concurrent.MVar as MVar
import           Control.Concurrent.Async

import           Control.Monad.IO.Class
import           System.Timeout (timeout)

-- import Debug.Trace


-- in us
timeoutLen :: Int
timeoutLen = 1000 * 1000

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
    Just (h, s) <- timeout timeoutLen $ run $ do
      append events "a"
      append events "b"
      h <- snapshot events
      s <- S.toList_ $ S.take 2 $ stream events 
      pure (h, s)
    s `shouldBe` h

concurrentContract :: (Monad m, MonadIO m) => (forall a . m a -> IO a) -> SpecWith (EventLog String m)
concurrentContract run = do
  it "should stream asynchronously" $ \events -> do
      putStrLn "Start stream async"
      mvar <- MVar.newEmptyMVar
      let expected = ["a", "b", "c"] :: [String]
      evs <- timeout timeoutLen $ withAsync (run $ S.mapM_ (liftIO . MVar.putMVar mvar) $ stream events) $ \_ -> run $ do
        liftIO $ putStrLn "Put stream async"
        flip mapM expected $ \ev -> do
          liftIO $ putStrLn $ "Put stream async " ++ show ev
          append events ev
          liftIO $ MVar.takeMVar mvar
      putStrLn $ "Done stream async" ++ show evs
      evs `shouldBe` Just expected

spec :: Spec
spec = do
  describe "EventLog" $ do
    describe "Pure" $ around withStateEvents $ logContract runState
    describe "STM" $ around withStmEvents $ logContract runStm
    describe "Pg" $ around withPgFromEnv $ logContract runPg
  describe "Conc" $ do
    describe "Pg" $ around withPgFromEnv $ concurrentContract runPg
    describe "STM" $ around withStmEvents $ concurrentContract runStm
    pure ()
