{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.OrdECM
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Expiring cache map using Ord keys with "Data.Map.Strict".
-- 

module Caching.ExpiringCacheMap.OrdECM (
    newECM,
    getECM,
    getStats,
    ECM
) where

import qualified Control.Concurrent.MVar as MV
import qualified Data.Map.Strict as M
import qualified Data.List as L

import Caching.ExpiringCacheMap.Internal (updateUses)
import Caching.ExpiringCacheMap.Types

newECM :: Ord k => (k -> IO v) -> (IO TimeUnits) -> Int -> Int -> ECMIncr -> ECMULength -> IO (ECM IO MV.MVar M.Map k v)
newECM retr gettime minimumkeep expirytime timecheckmodulo removalsize = do
  m'maps <- MV.newMVar $ ( M.empty, ([], 0), 0 )
  return (m'maps, retr, gettime, minimumkeep, expirytime, timecheckmodulo, removalsize, removalsize*2, MV.modifyMVar, MV.readMVar)

getECM :: (Monad m, Ord k) => ECM m mv M.Map k v -> k -> m v
getECM ecm id = do
  enter m'maps $
    \(maps, uses, incr) ->
      let incr' = incr + 1
       in if incr' < incr
            -- Word incrementor has cycled back to 0,
            -- so may as well clear the cache completely.
            then getECM' (M.empty, ([], 0), 0) (0+1)
            else getECM' (maps, uses, incr) incr'
  where
    
    getECM' (maps, uses, incr) incr' = do
      let uses' = updateUses uses id incr' compactlistsize (M.toList . M.fromList . reverse)
      case M.lookup id maps of
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
    
    (m'maps, retr, gettime, minimumkeep, expirytime, timecheckmodulo, removalsize, compactlistsize, enter, _ro) = ecm
    
    getKeepAndRemove =
      finalTup . splitAt minimumkeep . reverse . 
          sortI . map swap2 . M.toList . M.fromList . reverse
        where swap2 (a,b) = (b,a)
              finalTup (l1,l2) = 
                (map (\(c,k) -> (k,c)) l1, map (\(c,k) -> k) l2)
              sortI = L.sortBy (\(l,_) (r,_) -> compare l r)
    
    insertAndPerhapsRemoveSome id time r maps uses =
      if lcount >= removalsize
        then 
          let (keepuses, _removekeys) = getKeepAndRemove usesl
              newmaps = M.insert id (time, r) $! M.intersection maps $ M.fromList keepuses
           in (filterExpired time newmaps, (keepuses, L.length keepuses))
        else
          let newmaps = M.insert id (time, r) maps
           in (filterExpired time newmaps, uses)
      where
        (usesl, lcount) = uses
    
    filterExpired time =
      M.filter (\(accesstime, value) ->
                 (accesstime <= time) &&
                   (accesstime > (time - expirytime)))

getStats ecm = do
  (maps, uses, incr) <- ro m'uses
  return uses
  where
    (m'uses, retr, gettime, minimumkeep, expirytime, timecheckmodulo, removalsize, compactlistsize, _enter, ro) = ecm
