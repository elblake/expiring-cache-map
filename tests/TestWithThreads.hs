{-# LANGUAGE OverloadedStrings #-}

--
-- Test with threads
--

-- module TestWithThreads where

import qualified TestOrdECMWithThreads as OrdTest
import qualified TestHashECMWithThreads as HashTest

testWithThreads = do
  HashTest.testWithThreads
  OrdTest.testWithThreads
  return ()

main = testWithThreads
