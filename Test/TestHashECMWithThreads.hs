{-# LANGUAGE OverloadedStrings #-}

--
-- Test HashECM with threads
--

module Test.TestHashECMWithThreads where

-- Test
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Time.Clock.POSIX as POSIX (POSIXTime, getPOSIXTime)
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Control.Concurrent.MVar as MV
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Word (Word32)
import Data.Hashable (Hashable(..))

import Caching.ExpiringCacheMap.HashECM

testWithThreads = do
  ecm <- newECM
            (\id -> do LBS.putStrLn id; return [])
            (do time <- POSIX.getPOSIXTime
                return (round (time * 100)))
            6 10 120 6 :: IO (ECM IO MV.MVar HM.HashMap LBS.ByteString [Int])
  forkIO $ do
    mapM_ (\a -> do
      b <- getECM ecm "test.2"
      threadDelay 2
      return ())
      [0..400]
  forkIO $ do
    mapM_ (\a -> do
      b <- getECM ecm "test.5"
      threadDelay 5
      return ())
      [0..300]
  forkIO $ do
    mapM_ (\a -> do
      b <- getECM ecm "test.7"
      threadDelay 7
      return ())
      [0..300]
  forkIO $ do
    mapM_ (\a -> do
      b <- getECM ecm "test.11"
      threadDelay 11
      return ())
      [0..200]
  forkIO $ do
    mapM_ (\a -> do
      b <- getECM ecm "test.13"
      threadDelay 13
      return ())
      [0..200]
  forkIO $ do
    mapM_ (\a -> do
      b <- getECM ecm "test.17"
      threadDelay 17
      return ())
      [0..200]
  forkIO $ do
    mapM_ (\a -> do
      b <- getECM ecm "test.111"
      threadDelay 111
      return ())
      [0..20]
  forkIO $ do
    mapM_ (\a -> do
      b <- getECM ecm "test.109"
      threadDelay 109
      return ())
      [0..20]
  forkIO $ do
    mapM_ (\a -> do
      b <- getECM ecm "test.1091"
      threadDelay 1091
      return ())
      [0..5]
  threadDelay 2000000
  c <- getStats ecm
  putStrLn (show c)
  return ()
