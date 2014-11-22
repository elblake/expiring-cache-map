{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.Internal.Types
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Internal types.
-- 

module Caching.ExpiringCacheMap.Internal.Types (
    -- * Cache internals
    ECM(..),
    CacheState(..),
    ECMNewState,
    ECMEnterState,
    ECMReadState
) where

import qualified Control.Concurrent.MVar as MV
import Caching.ExpiringCacheMap.Utils.Types

type ECMNewState a b m k v = (CacheState m k v) -> a (b (CacheState m k v))

type ECMEnterState a b m k v = b (CacheState m k v) -> ((CacheState m k v) -> a ((CacheState m k v), v)) -> a v

type ECMReadState a b m k v = b (CacheState m k v) -> a (CacheState m k v)

-- | The cache state.
newtype CacheState m k v =
  CacheState (m k (TimeUnits, TimeUnits, v), ([(k, ECMIncr)], ECMULength), ECMIncr)

-- | The type that encapsulates a cache map.
newtype ECM a b m k v = ECM ( b (CacheState m k v),
                  k -> a v,
                  a TimeUnits,
                  ECMMapSize,
                  TimeUnits,
                  ECMIncr,
                  ECMULength,
                  ECMULength,
                  ECMEnterState a b m k v,
                  ECMReadState a b m k v)
