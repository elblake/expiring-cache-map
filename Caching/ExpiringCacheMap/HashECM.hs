{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.HashECM
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Expiring cache map using Hashable keys with "Data.HashMap.Strict".
-- 

module Caching.ExpiringCacheMap.HashECM (
    newECM,
    getECM,
    getStats,
    ECM
) where

import qualified Control.Concurrent.MVar as MV
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Hashable (Hashable(..))

import Caching.ExpiringCacheMap.Internal (updateUses)
import Caching.ExpiringCacheMap.Types

newECM :: (Eq k, Hashable k) => (k -> IO v) -> (IO TimeUnits) -> Int -> Int -> ECMIncr -> ECMULength -> IO (ECM HM.HashMap k v)
newECM retr gettime minimumkeep expirytime timecheckmodulo removalsize = do
  m'maps <- MV.newMVar $ ( HM.empty, ([], 0), 0 )
  return (m'maps, retr, gettime, minimumkeep, expirytime, timecheckmodulo, removalsize, removalsize*2)

getECM :: (Eq k, Hashable k) => ECM HM.HashMap k v -> k -> IO v
getECM ecm id = do
  MV.modifyMVar m'maps $
    \(maps, uses, incr) ->
      let incr' = incr + 1
       in if incr' < incr
            -- Word incrementor has cycled back to 0,
            -- so may as well clear the cache completely.
            then getECM' (HM.empty, ([], 0), 0) (0+1)
            else getECM' (maps, uses, incr) incr'
  where
    getECM' (maps, uses, incr) incr' = do
      let uses' = updateUses uses id incr' compactlistsize (HM.toList . HM.fromList . reverse)
      case HM.lookup id maps of
        Nothing -> do
          r <- retr id
          time <- gettime
          let (newmaps,newuses) = insertAndPerhapsRemoveSome id time r maps uses'
          return $! ((newmaps, newuses, incr'), r)
        Just (accesstime, m) -> do
          if incr' `mod` timecheckmodulo == 0
            then do
              time <- gettime
              return ((filterExpired time maps, uses', incr'), m)
            else return ((maps, uses', incr'), m)
    
    (m'maps, retr, gettime, minimumkeep, expirytime, timecheckmodulo, removalsize, compactlistsize) = ecm
    
    getKeepAndRemove =
      finalTup . splitAt minimumkeep . reverse . 
          sortI . map swap2 . HM.toList . HM.fromList . reverse
        where swap2 (a,b) = (b,a)
              finalTup (l1,l2) = 
                (map (\(c,k) -> (k,c)) l1, map (\(c,k) -> k) l2)
              sortI = L.sortBy (\(l,_) (r,_) -> compare l r)
    
    insertAndPerhapsRemoveSome id time r maps uses =
      if lcount >= removalsize
        then 
          let (keepuses, _removekeys) = getKeepAndRemove usesl
              newmaps = HM.insert id (time, r) $! HM.intersection maps $ HM.fromList keepuses
           in (filterExpired time newmaps, (keepuses, L.length keepuses))
        else
          let newmaps = HM.insert id (time, r) maps
           in (filterExpired time newmaps, uses)
      where
        (usesl, lcount) = uses
    
    filterExpired time =
      HM.filter (\(accesstime, value) ->
                 (accesstime <= time) &&
                   (accesstime > (time - expirytime)))

getStats ecm = do
  (maps, uses, incr) <- MV.readMVar m'uses
  return uses
  where
    (m'uses, retr, gettime, minimumkeep, expirytime, timecheckmodulo, removalsize, compactlistsize) = ecm
