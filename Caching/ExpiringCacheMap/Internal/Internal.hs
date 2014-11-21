{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.Internal
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- A module with internal functions used in common by HashECM and OrdECM.
-- Assume these functions to change from version to version.
-- 

module Caching.ExpiringCacheMap.Internal.Internal (
    updateUses,
    detECM
) where

import qualified Data.List as L

import Caching.ExpiringCacheMap.Types

updateUses
  :: (Eq k) =>
     ([(k, ECMIncr)], ECMULength)
     -> k -> ECMIncr -> ECMULength -> ([(k, ECMIncr)] -> [(k, ECMIncr)]) -> ([(k, ECMIncr)], ECMULength)
{-# INLINE updateUses #-}
updateUses uses id incr' compactlistsize compactUses =
    case uses of
        (((id', _) : rest), lcount) | id' == id ->
          (((id', incr') : rest), lcount)
        ((latest : (id', _) : rest), lcount) | id' == id ->
          (((id', incr') : latest : rest), lcount)
        ((latest : latest' : (id', _) : rest), lcount) | id' == id ->
          (((id', incr') : latest : latest' : rest), lcount)
        (usesl, lcount) ->
          if lcount > compactlistsize
            then let newusesl = compactUses usesl
                  in ((id, incr') : newusesl, (L.length newusesl) + 1)
            else ((id, incr') : usesl, lcount+1)

detECM
  :: (Monad m, Eq k) =>
     Maybe (TimeUnits, v)
     -> m v
     -> ((TimeUnits, v) -> mp k (TimeUnits, v))
     -> ((TimeUnits, v) -> [(k, ECMIncr)] -> mp k (TimeUnits, v))
     -> ([(k, ECMIncr)] -> [(k, ECMIncr)])
     -> m TimeUnits
     -> (((TimeUnits, v) -> Bool)
         -> mp k (TimeUnits, v) -> mp k (TimeUnits, v))
     -> ([(k, ECMIncr)], ECMULength)
     -> ECMIncr
     -> TimeUnits
     -> ECMIncr
     -> mp k (TimeUnits, v)
     -> ECMMapSize
     -> ECMULength
     -> m (CacheState mp k v, v)
{-# INLINE detECM #-}
detECM result retr_id insert_id1 insert_id2 mnub gettime filt uses' incr' expirytime timecheckmodulo maps minimumkeep removalsize = 
    case result of
        Nothing -> do
          r <- retr_id
          time <- gettime
          let (newmaps,newuses) = insertAndPerhapsRemoveSome time r uses'
          return $! (CacheState (newmaps, newuses, incr'), r)
        Just (accesstime, m) -> do
          if incr' `mod` timecheckmodulo == 0
            then do
              time <- gettime
              return (CacheState (filterExpired time maps, uses', incr'), m)
            else return (CacheState (maps, uses', incr'), m)
  where
  
    getKeepAndRemove =
      finalTup . splitAt minimumkeep . reverse . 
          sortI . map swap2 . mnub
        where swap2 (a,b) = (b,a)
              finalTup (l1,l2) = 
                (map (\(c,k) -> (k,c)) l1, map (\(c,k) -> k) l2)
              sortI = L.sortBy (\(l,_) (r,_) -> compare l r)
    
    insertAndPerhapsRemoveSome time r uses =
      if lcount >= removalsize
        then 
          let (keepuses, _removekeys) = getKeepAndRemove usesl
              newmaps = insert_id2 (time, r) keepuses
           in (filterExpired time newmaps, (keepuses, L.length keepuses))
        else
          let newmaps = insert_id1 (time, r)
           in (filterExpired time newmaps, uses)
      where
        (usesl, lcount) = uses
    
    filterExpired time =
      filt (\(accesstime, value) ->
                 (accesstime <= time) &&
                   (accesstime > (time - expirytime)))
