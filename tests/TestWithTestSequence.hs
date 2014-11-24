-- module TestWithTestSequence where

import qualified TestOrdECMWithTestSequence as OrdTest
import qualified TestHashECMWithTestSequence as HashTest

main = do
  HashTest.testWithTestSequence
  OrdTest.testWithTestSequence
  return ()