{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.Types
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Types common to "Caching.ExpiringCacheMap.OrdECM" and "Caching.ExpiringCacheMap.HashECM".
-- 

module Caching.ExpiringCacheMap.Types (
    -- * Cache encapsulation
    ECM(..),
    -- * Types
    TimeUnits,
    ECMMapSize,
    ECMULength,
    ECMIncr,
    -- * Types for state function
    ECMNewState,
    ECMEnterState,
    ECMReadState,
    -- * Internal
    CacheState(..)
) where

import qualified Control.Concurrent.MVar as MV
import Data.Word (Word32)

-- | Integer involved in the time units used to determine when an item expires.
type TimeUnits = Int

-- | Integer involved in the size of a key-value map.
type ECMMapSize = Int

-- | Integer involved in the length of the usage history list.
type ECMULength = Int

-- | Unsigned integer ('Word32') involved in the cache state incrementing accumulator.
type ECMIncr = Word32

-- | The cache state.
newtype CacheState m k v = CacheState (m k (TimeUnits, v), ([(k, ECMIncr)], ECMULength), ECMIncr)

type ECMNewState a b m k v = (CacheState m k v) -> a (b (CacheState m k v))

type ECMEnterState a b m k v = b (CacheState m k v) -> ((CacheState m k v) -> a ((CacheState m k v), v)) -> a v

type ECMReadState a b m k v = b (CacheState m k v) -> a (CacheState m k v)

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
