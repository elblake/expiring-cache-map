{-# LANGUAGE OverloadedStrings #-}

module PerfTest where

import Caching.ExpiringCacheMap.HashECM (newECMIO, lookupECM, CacheSettings(..), consistentDuration)

import qualified Data.Time.Clock.POSIX as POSIX (POSIXTime, getPOSIXTime)
import qualified Data.ByteString.Char8 as BS
import System.IO (withFile, IOMode(ReadMode))
import Data.Time.Clock

main = do

  filecache <- newECMIO
        (consistentDuration 100
          (\state id -> do BS.putStrLn "Reading a file again..."
                           return (state, []))) -- : >>= \r -> return $! r
        (do time <- POSIX.getPOSIXTime
            return (round (time * 100)))
        12000 -- Value to modulo with cache state accumulator to determine time check frequency.
        (CacheWithLRUList
          6     -- Expected size of key-value map when removing elements.
          6     -- Size at when to remove items from key-value map.
          12 )
  
  timer_1 <- getCurrentTime
  mapM_ (\_ -> do
    b <- lookupECM filecache ("file1" :: BS.ByteString)
    return ()
    )
    [0 .. 400000]
  timer_2 <- getCurrentTime
  putStrLn $ "time taken: " ++ (show $ diffUTCTime timer_2 timer_1)

  return ()
