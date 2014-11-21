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
    TimeUnits,
    ECMMapSize,
    ECMULength,
    ECMIncr,
    ECM(..),
    CacheState(..)
) where

import qualified Control.Concurrent.MVar as MV
import Data.Word (Word32)

type TimeUnits = Int
type ECMMapSize = Int
type ECMULength = Int
type ECMIncr = Word32

newtype CacheState m k v = CacheState (m k (TimeUnits, v), ([(k, ECMIncr)], ECMULength), ECMIncr)

newtype ECM a b m k v = ECM ( b (CacheState m k v),
                  k -> a v,
                  a TimeUnits,
                  ECMMapSize,
                  TimeUnits,
                  ECMIncr,
                  ECMULength,
                  ECMULength,
                  b (CacheState m k v) -> ((CacheState m k v) -> a ((CacheState m k v), v)) -> a v,
                  b (CacheState m k v) -> a (CacheState m k v))
