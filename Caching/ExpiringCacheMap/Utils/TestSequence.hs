-- |
-- Module : Caching.ExpiringCacheMap.Utils.TestSequence
-- Copyright: (c) 2014 Edward L. Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- TestSequence Monad for testing caching behaviour.
--
--  test = do
--    runTestSequence (do
--      a <- getCurrentTime
--      if a == 0
--        then do b <- readNumber
--                return (a,b)
--        else do return (a,-8)) 0
--

module Caching.ExpiringCacheMap.Utils.TestSequence (
    runTestSequence,
    newTestSVar,
    enterTestSVar,
    readTestSVar,
    getCurrentTime,
    readNumber,
    TestSequenceEvents(..),
    TestSequenceState(..),
    TestSequence(..),
    TestSVar(..)
) where

import Data.Word (Word32)

data TestSequenceEvents = 
  GetVar Word32 |
  PutVar Word32 |
  GetTime Word32 |
  ReadNumber Int |
  HaveNumber Int
  deriving (Eq)

instance Show TestSequenceEvents where
  show (GetVar a)     = "GetVar " ++ (show a)
  show (PutVar a)     = "PutVar " ++ (show a)
  show (GetTime a)    = "GetTime " ++ (show a)
  show (ReadNumber a) = "ReadNumber " ++ (show a)
  show (HaveNumber a) = "HaveNumber " ++ (show a)


newtype TestSequenceState b =
  TestSequenceState (Word32, [TestSequenceEvents], Maybe b)
  
instance Show (TestSequenceState ct) where
  show (TestSequenceState (a,b,_)) =
    "TestSequenceState " ++ (show a) ++ " " ++ (show b)

newtype TestSequence b a =
  TestSequence (TestSequenceState b -> (TestSequenceState b, a))

newtype TestSVar a = TestSVar a


instance Monad (TestSequence a) where
  TestSequence fun >>= k =
    TestSequence
      (\state -> let (state', ret) = (fun state)
                     TestSequence fun' = k ret
                  in fun' state')
  return ret = 
    TestSequence $
      \(TestSequenceState (timer, hl, testsvar)) ->
       (TestSequenceState (timer+1,hl, testsvar), ret)

runTestSequence :: Show a => TestSequence b a -> IO (TestSequenceState b, a)
runTestSequence f = do
  let ret = (fun (TestSequenceState (0, [], Nothing)))
   in do putStrLn (show ret)
         return ret
  where
    TestSequence fun = (TestSequence
      (\(TestSequenceState (t, hl, testsvar)) ->
        (TestSequenceState (t+1, hl, testsvar), ()))) >> f

newTestSVar :: a -> TestSequence a (TestSVar a)
newTestSVar var = TestSequence $
  \(TestSequenceState (timer, hl, Nothing)) ->
   (TestSequenceState (timer+1, hl, Just var), TestSVar var)

enterTestSVar :: TestSVar a -> (a -> TestSequence a (a,b)) -> TestSequence a b
enterTestSVar testsvar fun = do
  teststate <- readTestSVar testsvar
  (teststate',passalong) <- fun teststate
  putTestSVar testsvar teststate'
  return passalong

-- 'putTestSVar' is used along with 'readTestSVar' to implement enterTestSVar.
--
putTestSVar :: TestSVar a -> a -> TestSequence a a
putTestSVar _testsvar testsvar' = TestSequence $
  \(TestSequenceState (timer, hl, testsvar)) ->
   (TestSequenceState (timer+1, (PutVar timer) : hl, Just testsvar'),
      case testsvar of
        Nothing -> testsvar'
        Just testsvar'' -> testsvar'')

readTestSVar :: TestSVar a -> TestSequence a a
readTestSVar _testsvar = TestSequence $
  \(TestSequenceState (timer, hl, Just testsvar)) ->
   (TestSequenceState (timer+1, (GetVar timer) : hl, Just testsvar), testsvar)

getCurrentTime :: TestSequence a Int
getCurrentTime = TestSequence $
  \(TestSequenceState (timer, hl, testsvar)) ->
   (TestSequenceState (timer+1, (GetTime timer) : hl, testsvar), fromIntegral timer)

readNumber :: TestSequence a Int
readNumber = TestSequence $
  \(TestSequenceState (timer, hl, testsvar)) ->
    let number = fromIntegral timer
     in (TestSequenceState (timer+1, (ReadNumber number) : hl, testsvar), number)

haveNumber :: Int -> TestSequence a ()
haveNumber number = TestSequence $
  \(TestSequenceState (timer, hl, testsvar)) ->
   (TestSequenceState (timer+1, (HaveNumber number) : hl, testsvar), ())



