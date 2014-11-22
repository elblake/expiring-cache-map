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
    -- * Configuration
    CacheSettings(..),
    -- * Cache encapsulation
    ECM,
    CacheState,
    -- * Types
    TimeUnits,
    ECMMapSize,
    ECMULength,
    ECMIncr,
    -- * Types for state function
    ECMNewState,
    ECMEnterState,
    ECMReadState,
) where

import Caching.ExpiringCacheMap.Utils.Types
import Caching.ExpiringCacheMap.Internal.Types

-- expirytime 
data CacheSettings =
  CacheWithLRUList {
    mapsize :: ECMMapSize,
    removalsize :: ECMULength,
    compactlistsize :: ECMULength
  }
