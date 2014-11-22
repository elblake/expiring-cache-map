{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.OrdECM
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- A cache that holds values for a length of time that uses 'Ord' keys with 
-- "Data.Map.Strict".
-- 
-- > 

module Caching.ExpiringCacheMap.OrdECM (
    -- * Create cache
    newECMIO,
    newECMForM,
    consistentDuration,
    
    -- * Request value from cache
    lookupECM,
    lookupECMUpd,
    
    -- * Type
    ECM,
    CacheSettings(..)
    
    -- -- * Miscellaneous
    , getStats
) where

import qualified Control.Concurrent.MVar as MV
import qualified Data.Map.Strict as M
import qualified Data.List as L

import Caching.ExpiringCacheMap.Internal.Internal (updateUses, detECM)
import Caching.ExpiringCacheMap.Types
import Caching.ExpiringCacheMap.Internal.Types

-- | Create a new expiring cache for retrieving uncached values via 'IO'
-- interaction (such as in the case of reading a file from disk), with
-- a shared state lock via an 'MV.MVar' to manage cache state.
--
newECMIO :: Ord k => (k -> IO (TimeUnits, v)) -> (IO TimeUnits)
  -> ECMIncr 
  -> CacheSettings
    -> IO (ECM IO MV.MVar M.Map k v)
newECMIO retr gettime timecheckmodulo settings = do
  newECMForM retr gettime timecheckmodulo settings
    MV.newMVar MV.modifyMVar MV.readMVar

-- | Create a new expiring cache along arbitrary monads with provided
-- functions to create cache state in 'Monad' m2, and modify and read
-- cache state in 'Monad' m1.
--
newECMForM :: (Monad m1, Monad m2) => Ord k => (k -> m1 (TimeUnits, v)) -> (m1 TimeUnits)
  -> ECMIncr
  -> CacheSettings
  -> ECMNewState m2 mv M.Map k v
  -> ECMEnterState m1 mv M.Map k v
  -> ECMReadState m1 mv M.Map k v
    -> m2 (ECM m1 mv M.Map k v)
newECMForM retr gettime timecheckmodulo (CacheWithLRUList minimumkeep removalsize compactlistsize)
           newstate enterstate readstate = do
  m'maps <- newstate $ CacheState ( M.empty, ([], 0), 0 )
  return $ ECM ( m'maps, retr, gettime, minimumkeep, timecheckmodulo, removalsize,
                 compactlistsize, enterstate, readstate )

-- | Request a value associated with a key from the cache.
--
--  * If the value is not in the cache, it will be requested through the 
--    function defined through 'newECM', its computation returned and the
--    value stored in the cache state map.
--
--  * If the value is in the cache and a new time is computed with the
--    function defined through 'newECM' in the same lookup, and the value
--    has been determined to have since expired, it will be returned
--    regardless for this computation and the key will be removed along
--    with other expired values from the cache state map. Use 'lookupECMUpd'
--    to have the opposite behaviour.
--
--  * If the value is in the cache and has not expired, it will be returned.
--
-- Every lookupECM computation increments an accumulator in the cache state which 
-- is used to keep track of the succession of key accesses. This history of
-- key accesses is then used to remove entries from the cache back down to a
-- minimum size. Also, when the modulo of the accumulator and the modulo value 
-- computes to 0, the time request function defined with 'newECM' is invoked 
-- for the current time to determine of which if any of the entries in the
-- cache state map needs to be removed.
--
-- As the accumulator is a bound unsigned integer, when the accumulator 
-- increments back to 0, the cache state is completely cleared.
-- 
lookupECM :: (Monad m, Ord k) => ECM m mv M.Map k v -> k -> m v
lookupECM ecm id = do
  enter m'maps $
    \(CacheState (maps, uses, incr)) ->
      let incr' = incr + 1
       in if incr' < incr
            -- Word incrementor has cycled back to 0,
            -- so may as well clear the cache completely.
            then lookupECM' (M.empty, ([], 0), 0) (0+1)
            else lookupECM' (maps, uses, incr) incr'
  where
    
    ECM ( m'maps, retr, gettime, minimumkeep, timecheckmodulo, removalsize,
          compactlistsize, enter, _ro ) = ecm
  
    mnub = M.toList . M.fromList . reverse
    lookupECM' (maps, uses, incr) incr' = do
      let uses' = updateUses uses id incr' compactlistsize mnub
      (ret, _) <-
          detECM (M.lookup id maps) (retr id)
            (\time_r -> M.insert id time_r maps)
            (\time_r keepuses -> M.insert id time_r $! M.intersection maps $ M.fromList keepuses)
            mnub
            gettime
            M.filter
            uses' incr' timecheckmodulo maps minimumkeep removalsize
      return ret


-- | Request a value associated with a key from the cache.
-- 
-- This function differs from 'lookupECM' only in the case that the value being
-- requested also causes a new time to have been computed during the same lookup,
-- and have been found to be out of date. When the condition happens, a new
-- version of the value will be requested with the function defined through
-- 'newECM' and possibly some removals.
-- 
lookupECMUpd :: (Monad m, Ord k) => ECM m mv M.Map k v -> k -> m v
lookupECMUpd ecm id = do
  enter m'maps $
    \(CacheState (maps, uses, incr)) ->
      let incr' = incr + 1
       in if incr' < incr
            -- Word incrementor has cycled back to 0,
            -- so may as well clear the cache completely.
            then lookupECM' (M.empty, ([], 0), 0) (0+1)
            else lookupECM' (maps, uses, incr) incr'
  where
    
    ECM ( m'maps, retr, gettime, minimumkeep, timecheckmodulo, removalsize,
          compactlistsize, enter, _ro ) = ecm
  
    mnub = M.toList . M.fromList . reverse
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
      detECM (M.lookup id maps) (retr id)
        (\time_r -> M.insert id time_r maps)
        (\time_r keepuses -> M.insert id time_r $! M.intersection maps $ M.fromList keepuses)
        mnub
        gettime
        M.filter
        uses' incr' timecheckmodulo maps minimumkeep removalsize
    

-- | Used with 'newECMIO' or 'newECMForM' to provide a consistent duration for requested values.
consistentDuration :: (Monad m, Ord k) => TimeUnits -> (k -> m v) -> (k -> m (TimeUnits, v))
consistentDuration duration fun =
  \id -> do
    ret <- fun id
    return (duration, ret)


-- Debugging function
--
getStats ecm = do
  CacheState (maps, uses, incr) <- ro m'uses
  return uses
  where
    ECM ( m'uses, _retr, _gettime, _minimumkeep, _timecheckmodulo, _removalsize,
          _compactlistsize, _enter, ro ) = ecm

