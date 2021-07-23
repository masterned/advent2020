module Day2.Test.PasswordPhilosophy where

import Prelude
import Control.Monad.Free (Free)
import Data.Array.NonEmpty (fromArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Day2.PasswordPhilosophy (breakApart, toPasswordSpec)
import Effect (Effect)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

ex1 :: String
ex1 = "1-3 a: abcde"

ex2 :: String
ex2 = "1-3 b: cdefg"

ex3 :: String
ex3 = "2-9 c: ccccccccc"

testBreakApart :: Free TestF Unit
testBreakApart =
  suite "breakApart" do
    test "ex 1" do
      equal (Right (fromArray [ Just "1-3 a: abcde", Just "1", Just "3", Just "a", Just "abcde" ])) $ breakApart ex1
    test "ex 2" do
      equal (Right (fromArray [ Just "1-3 b: cdefg", Just "1", Just "3", Just "b", Just "cdefg" ])) $ breakApart ex2
    test "ex 3" do
      equal (Right (fromArray [ Just "2-9 c: ccccccccc", Just "2", Just "9", Just "c", Just "ccccccccc" ])) $ breakApart ex3

testToPasswordSpec :: Free TestF Unit
testToPasswordSpec =
  suite "toPasswordSpec" do
    test "ex 1" do
      equal { min: 1, max: 3, character: 'a', password: "abcde" } $ toPasswordSpec ex1
    test "ex 2" do
      equal { min: 1, max: 3, character: 'b', password: "cdefg" } $ toPasswordSpec ex2
    test "ex 3" do
      equal { min: 2, max: 9, character: 'c', password: "ccccccccc" } $ toPasswordSpec ex3

runTests :: Effect Unit
runTests =
  runTest do
    testBreakApart
    testToPasswordSpec
