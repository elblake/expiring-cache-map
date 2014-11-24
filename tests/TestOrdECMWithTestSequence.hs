{-# LANGUAGE OverloadedStrings #-}

module TestOrdECMWithTestSequence where

import Caching.ExpiringCacheMap.OrdECM (newECMForM, lookupECM, CacheSettings(..), consistentDuration)
import qualified Caching.ExpiringCacheMap.Utils.TestSequence as TestSeq

import qualified Data.ByteString.Char8 as BS

numberEventsOnly a =
  case a of
    TestSeq.ReadNumber _ -> True
    TestSeq.HaveNumber _ -> True
    _ -> False

testWithTestSequence = do
  (TestSeq.TestSequenceState (_, events, _), return_value) <- TestSeq.runTestSequence test'
  case (filter numberEventsOnly . reverse) events of
    [ TestSeq.ReadNumber numr1, TestSeq.HaveNumber numh1,
      TestSeq.ReadNumber numr2, TestSeq.HaveNumber numh2 ]
      | numr1 == numh1 && numr2 == numh2 -> do
        (putStrLn . show . filter numberEventsOnly . reverse) events
        return ()
  where
    test' = do
      filecache <- newECMForM
            (consistentDuration 100 -- Duration between access and expiry time of each item, no state needed.
              (\state _id -> do number <- TestSeq.readNumber
                                return (state, number)))
            (TestSeq.getCurrentTime >>= return)
            12000 -- Time check frequency: (accumulator `mod` this_number) == 0.
            (CacheWithLRUList 
              6   -- Expected size of key-value map when removing elements.
              6   -- Size of list when to remove items from key-value map.
              12  -- Size of list when to compact
              )
            TestSeq.newTestSVar TestSeq.enterTestSVar TestSeq.readTestSVar
      
      -- Use lookupECM whenever the contents of "file1" is needed.
      b <- lookupECM filecache ("file1" :: BS.ByteString)
      TestSeq.haveNumber b
      b <- lookupECM filecache "file1"
      b <- lookupECM filecache "file2"
      TestSeq.haveNumber b
      return b

main = testWithTestSequence

 