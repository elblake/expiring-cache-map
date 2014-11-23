{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.HashECM
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- A cache that holds values for a length of time that uses 'Hashable' keys
-- with "Data.HashMap.Strict".
--
-- An example of creating a cache for accessing files:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Example where
-- > 
-- > import Caching.ExpiringCacheMap.HashECM (newECMIO, lookupECM, CacheSettings(..), consistentDuration)
-- > 
-- > import qualified Data.Time.Clock.POSIX as POSIX (POSIXTime, getPOSIXTime)
-- > import qualified Data.ByteString.Char8 as BS
-- > import System.IO (withFile, IOMode(ReadMode))
-- > 
-- > example = do
-- >   filecache <- newECMIO
-- >         (consistentDuration 100 -- Duration between access and expiry time of each item
-- >           (\id -> do BS.putStrLn "Reading a file again..."
-- >                      withFile (case id :: BS.ByteString of
-- >                                  "file1" -> "file1.txt"
-- >                                  "file2" -> "file2.txt")
-- >                               ReadMode $
-- >                        \fh -> BS.hGetContents fh >>= (return $!)))
-- >         (do time <- POSIX.getPOSIXTime
-- >             return (round (time * 100)))
-- >         12000 -- Time check frequency: (accumulator `mod` this_number) == 0.
-- >         (CacheWithLRUList
-- >           6     -- Expected size of key-value map when removing elements.
-- >           6     -- Size of list when to remove items from key-value map.
-- >           12    -- Size of list when to compact
-- >           )
-- >   
-- >   -- Use lookupECM whenever the contents of "file1" is needed.
-- >   b <- lookupECM filecache "file1"
-- >   BS.putStrLn b
-- >   return ()
-- > 
-- 

module Caching.ExpiringCacheMap.HashECM (
    
    -- * Create cache
    newECMIO,
    newECMForM,
    consistentDuration,
    
    -- * Request value from cache
    lookupECM,
    
    -- * Type
    ECM,
    CacheSettings(..)
) where

import qualified Control.Concurrent.MVar as MV
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Hashable (Hashable(..))

import Caching.ExpiringCacheMap.Internal.Internal (updateUses, detECM)
import Caching.ExpiringCacheMap.Types
import Caching.ExpiringCacheMap.Internal.Types

-- | Create a new expiring cache for retrieving uncached values via 'IO'
-- interaction (such as in the case of reading a file from disk), with 
-- a shared state lock via an 'MV.MVar' to manage cache state.
-- 
newECMIO :: (Eq k, Hashable k) => (k -> IO (TimeUnits, v)) -> (IO TimeUnits)
  -> ECMIncr 
  -> CacheSettings
    -> IO (ECM IO MV.MVar HM.HashMap k v)
newECMIO retr gettime timecheckmodulo cachesettings = do
  newECMForM retr gettime timecheckmodulo cachesettings
    MV.newMVar MV.modifyMVar MV.readMVar
  
-- | Create a new expiring cache along arbitrary monads with provided
-- functions to create cache state in 'Monad' m2, and modify and read
-- cache state in 'Monad' m1.
--
newECMForM :: (Monad m1, Monad m2) => (Eq k, Hashable k) => (k -> m1 (TimeUnits, v)) -> (m1 TimeUnits)
  -> ECMIncr 
  -> CacheSettings
  -> ECMNewState m2 mv HM.HashMap k v
  -> ECMEnterState m1 mv HM.HashMap k v
  -> ECMReadState m1 mv HM.HashMap k v
    -> m2 (ECM m1 mv HM.HashMap k v)
newECMForM retr gettime timecheckmodulo (CacheWithLRUList minimumkeep removalsize compactlistsize)
           newstate enterstate readstate = do
  m'maps <- newstate $ CacheState ( HM.empty, ([], 0), 0 )
  return $ ECM ( m'maps, retr, gettime, minimumkeep, 
                 timecheckmodulo, removalsize, compactlistsize,
                 enterstate, readstate )


-- | Request a value associated with a key from the cache.
--
--  * If the value is not in the cache, the value will be requested through the 
--    function defined when the 'ECM' value was created, its computation
--    returned and the value stored in the cache state map.
-- 
--  * If the value is in the cache and has not expired, it will be returned.
-- 
--  * If the value is in the cache and a new time is computed in the same
--    lookup, and the value has been determined to have since expired, it 
--    will be discarded and a new value will be requested for this computation.
--
-- Every 'lookupECM' computation increments an accumulator in the cache state
-- which is used to keep track of the succession of key accesses. This history 
-- of key accesses is then used to remove entries from the cache back down to 
-- a minimum size. Also, when the modulo of the accumulator and the modulo 
-- value computes to 0, the time request function defined when the 'ECM' value
-- was created is invoked for the current time to determine of which if any of
-- the entries in the cache state map needs to be removed. In some cases the 
-- accumulator may get incremented more than once in a 'lookupECM' computation.
--
-- As the accumulator is a bound unsigned integer, when the accumulator
-- increments back to 0, the cache state is completely cleared.
-- 
lookupECM :: (Monad m, Eq k, Hashable k) => ECM m mv HM.HashMap k v -> k -> m v
lookupECM ecm id = do
  enter m'maps $
    \(CacheState (maps, uses, incr)) ->
      let incr' = incr + 1
       in if incr' < incr
            -- Word incrementor has cycled back to 0,
            -- so may as well clear the cache completely.
            then lookupECM' (HM.empty, ([], 0), 0) (0+1)
            else lookupECM' (maps, uses, incr) incr'
  where
  
    ECM ( m'maps, retr, gettime, minimumkeep, timecheckmodulo, removalsize, 
          compactlistsize, enter, _ro ) = ecm
    
    mnub = HM.toList . HM.fromList . reverse
    lookupECM' (maps, uses, incr) incr' = do
      let uses' = updateUses uses id incr' compactlistsize mnub
      (ret, do_again) <- det maps uses' incr'
      if do_again
        then do let (CacheState (maps', uses'', incr''), _) = ret
                    uses''' = updateUses uses'' id incr'' compactlistsize mnub
                (ret', _) <- det maps' uses''' incr''
                return ret'
        else return ret
    
    det maps uses' incr' =
      detECM (HM.lookup id maps) (retr id)
        (\time_r -> HM.insert id time_r maps)
        (\time_r keepuses -> HM.insert id time_r $! HM.intersection maps $ HM.fromList keepuses)
        mnub
        gettime
        HM.filter
        uses' incr' timecheckmodulo maps minimumkeep removalsize


{-
-- This function differs from 'lookupECM' only in the case that the value
-- being requested also causes a new time to have been computed during the 
-- same lookup, and have been found to be out of date. When the condition 
-- happens, this function returns the old cached value without attempting
-- to request a new value, despite being out of date. However, it does
-- clear the key from the key-value store for the next request.
-- 
lookupECMUse :: (Monad m, Eq k, Hashable k) => ECM m mv HM.HashMap k v -> k -> m v
lookupECMUse ecm id = do
  enter m'maps $
    \(CacheState (maps, uses, incr)) ->
      let incr' = incr + 1
       in if incr' < incr
            -- Word incrementor has cycled back to 0,
            -- so may as well clear the cache completely.
            then lookupECM' (HM.empty, ([], 0), 0) (0+1)
            else lookupECM' (maps, uses, incr) incr'
  where
  
    ECM ( m'maps, retr, gettime, minimumkeep, timecheckmodulo, removalsize, 
          compactlistsize, enter, _ro ) = ecm
    
    mnub = HM.toList . HM.fromList . reverse
    lookupECM' (maps, uses, incr) incr' = do
      let uses' = updateUses uses id incr' compactlistsize mnub
      (ret, _) <-
          detECM (HM.lookup id maps) (retr id)
            (\time_r -> HM.insert id time_r maps)
            (\time_r keepuses -> HM.insert id time_r $! HM.intersection maps $ HM.fromList keepuses)
            mnub
            gettime
            HM.filter
            uses' incr' timecheckmodulo maps minimumkeep removalsize
      return ret
-}


-- | Used with 'newECMIO' or 'newECMForM' to provide a consistent duration for requested values.
consistentDuration :: (Monad m, Eq k, Hashable k) => TimeUnits -> (k -> m v) -> (k -> m (TimeUnits, v))
consistentDuration duration fun =
  \id -> do
    ret <- fun id
    return (duration, ret)

