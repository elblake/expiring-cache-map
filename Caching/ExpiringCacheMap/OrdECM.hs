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
    
    -- * Request value from cache
    getECM,
    
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
newECMIO :: Ord k => (k -> IO v) -> (IO TimeUnits)
  -> ECMIncr -> TimeUnits -- -- -> ECMMapSize -> ECMULength
  -> CacheSettings
    -> IO (ECM IO MV.MVar M.Map k v)
newECMIO retr gettime timecheckmodulo expirytime settings = do
  newECMForM retr gettime timecheckmodulo expirytime settings
    MV.newMVar MV.modifyMVar MV.readMVar

-- | Create a new expiring cache along arbitrary monads with provided
-- functions to create cache state in 'Monad' m2, and modify and read
-- cache state in 'Monad' m1.
--
newECMForM :: (Monad m1, Monad m2) => Ord k => (k -> m1 v) -> (m1 TimeUnits)
  -> ECMIncr -> TimeUnits -- -> ECMMapSize -> ECMULength
  -> CacheSettings
  -> ECMNewState m2 mv M.Map k v
  -> ECMEnterState m1 mv M.Map k v
  -> ECMReadState m1 mv M.Map k v
    -> m2 (ECM m1 mv M.Map k v)
newECMForM retr gettime timecheckmodulo expirytime (CacheWithLRUList minimumkeep removalsize compactlistsize)
           newstate enterstate readstate = do
  m'maps <- newstate $ CacheState ( M.empty, ([], 0), 0 )
  return $ ECM ( m'maps, retr, gettime, minimumkeep, expirytime,
                 timecheckmodulo, removalsize, removalsize*2,
                 enterstate, readstate )

-- | Request a value associated with a key from the cache.
--
--  * If the value is not in the cache, it will be requested through the 
--    function defined through 'newECM', its computation returned and the
--    value stored in the cache state map.
--
--  * If the value is in the cache, the modulo of the cache accumulator and
--    the modulo value equates to 0 which causes a time request with the
--    function defined through 'newECM', and the value has been determined
--    to have since expired, it will be returned regardless for this 
--    computation and the key will be removed along with other expired
--    values from the cache state map.
--
--  * If the value is in the cache and has not expired, it will be returned.
--
-- Every getECM computation increments an accumulator in the cache state which 
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
getECM :: (Monad m, Ord k) => ECM m mv M.Map k v -> k -> m v
getECM ecm id = do
  enter m'maps $
    \(CacheState (maps, uses, incr)) ->
      let incr' = incr + 1
       in if incr' < incr
            -- Word incrementor has cycled back to 0,
            -- so may as well clear the cache completely.
            then getECM' (M.empty, ([], 0), 0) (0+1)
            else getECM' (maps, uses, incr) incr'
  where
    
    ECM ( m'maps, retr, gettime, minimumkeep, expirytime,
          timecheckmodulo, removalsize, compactlistsize,
          enter, _ro ) = ecm
  
    mnub = M.toList . M.fromList . reverse
    getECM' (maps, uses, incr) incr' = do
      let uses' = updateUses uses id incr' compactlistsize mnub
      detECM (M.lookup id maps) (retr id)
        (\time_r -> M.insert id time_r maps)
        (\time_r keepuses -> M.insert id time_r $! M.intersection maps $ M.fromList keepuses)
        mnub
        gettime
        M.filter
        uses' incr' timecheckmodulo expirytime maps minimumkeep removalsize

-- Debugging function
--
getStats ecm = do
  CacheState (maps, uses, incr) <- ro m'uses
  return uses
  where
    ECM ( m'uses, _retr, _gettime, _minimumkeep, _expirytime,
          _timecheckmodulo, _removalsize, _compactlistsize,
          _enter, ro ) = ecm
