{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Caching.ExpiringCacheMap.Internal
-- Copyright: (c) 2014 Edward Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Module functions mainly used by HashECM and OrdECM.
-- 

module Caching.ExpiringCacheMap.Internal (
    updateUses
) where

import qualified Data.List as L
-- import Data.Word (Word32)

import Caching.ExpiringCacheMap.Types

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
