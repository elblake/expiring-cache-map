{-# LANGUAGE OverloadedStrings #-}

--
-- Test OrdECM with threads
--

module Test.TestOrdECMWithThreads where

import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Time.Clock.POSIX as POSIX (POSIXTime, getPOSIXTime)
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Control.Concurrent.MVar as MV
import qualified Data.Map as M

import Caching.ExpiringCacheMap.OrdECM
import Caching.ExpiringCacheMap.Internal.Internal (getStatsString)

testWithThreads = do
  ecm <- newECMIO
            (consistentDuration 10
              (\id -> do LBS.putStrLn id; return []))
            (do time <- POSIX.getPOSIXTime
                return (round (time * 100)))
            120
            (CacheWithLRUList 6 6 12 ) :: IO (ECM IO MV.MVar M.Map LBS.ByteString [Int])
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.2"
      threadDelay 2
      return ())
      [0..400]
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.5"
      threadDelay 5
      return ())
      [0..300]
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.7"
      threadDelay 7
      return ())
      [0..300]
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.11"
      threadDelay 11
      return ())
      [0..200]
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.13"
      threadDelay 13
      return ())
      [0..200]
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.17"
      threadDelay 17
      return ())
      [0..200]
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.111"
      threadDelay 111
      return ())
      [0..20]
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.109"
      threadDelay 109
      return ())
      [0..20]
  forkIO $ do
    mapM_ (\a -> do
      b <- lookupECM ecm "test.1091"
      threadDelay 1091
      return ())
      [0..5]
  threadDelay 2000000
  c <- getStatsString ecm
  putStrLn c
  return ()
