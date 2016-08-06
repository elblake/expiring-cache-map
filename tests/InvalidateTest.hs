{-# LANGUAGE OverloadedStrings #-}

module InvalidateTest where

import Caching.ExpiringCacheMap.HashECM (newECMIO, lookupECM, CacheSettings(..), consistentDuration)
import qualified Caching.ExpiringCacheMap.HashECM as ECM (invalidate, invalidateCache, keysCached, keysNotExpired)

import qualified Data.Time.Clock.POSIX as POSIX (POSIXTime, getPOSIXTime)
import qualified Data.ByteString.Char8 as BS
import System.IO (withFile, IOMode(ReadMode))
import Data.Time.Clock
import Control.Concurrent (threadDelay)

main = do

  test1
  test2
  return ()


test1 = do
  filecache1 <- newECMIO
        (consistentDuration 100
          (\state id -> do BS.putStrLn "Reading a file again..."
                           return (state, ("file contents" :: BS.ByteString) )))
        (do time <- POSIX.getPOSIXTime
            return (round (time * 100)))
        12000 -- Value to modulo with cache state accumulator to determine time check frequency.
        (CacheWithLRUList
          6     -- Expected size of key-value map when removing elements.
          6     -- Size at when to remove items from key-value map.
          12 )
  
  b <- lookupECM filecache1 ("file1" :: BS.ByteString)
  b <- lookupECM filecache1 "file2"
  b <- lookupECM filecache1 "file3"
  l <- ECM.keysCached filecache1
  putStrLn $ show l
  
  -- Repeat
  b <- lookupECM filecache1 "file2"
  b <- lookupECM filecache1 "file3"
  
  l <- ECM.keysCached filecache1
  putStrLn $ show l
  
  v <- ECM.invalidate filecache1 "file2"
  putStrLn $ show v
  l <- ECM.keysCached filecache1
  putStrLn $ show l
  
  b <- lookupECM filecache1 "file2"
  b <- lookupECM filecache1 "file3"
  l <- ECM.keysCached filecache1
  putStrLn $ show l
  
  kv <- ECM.invalidateCache filecache1
  putStrLn $ show kv
  l <- ECM.keysCached filecache1
  putStrLn $ show l
  return ()


test2 = do
  
  filecache2 <- newECMIO
        (consistentDuration 0
          (\state id -> do BS.putStrLn "Reading a file again..."
                           return (state, ("file contents" :: BS.ByteString) )))
        (do time <- POSIX.getPOSIXTime
            return (round (time * 100)))
        1 -- Value to modulo with cache state accumulator to determine time check frequency.
        (CacheWithLRUList
          6     -- Expected size of key-value map when removing elements.
          6     -- Size at when to remove items from key-value map.
          12 )
  
  b <- lookupECM filecache2 ("file1" :: BS.ByteString)
  b <- lookupECM filecache2 "file2"
  b <- lookupECM filecache2 "file3"
  l <- ECM.keysCached filecache2
  putStrLn $ show l
  threadDelay 2
  l <- ECM.keysNotExpired filecache2
  putStrLn $ show l
  
  return ()
