module Day1.Test.ReportRepair where

import Prelude
import Control.Monad.Free (Free)
import Data.Input (toIntArray)
import Data.Maybe (Maybe(..))
import Day1.ReportRepair (findLuckyPair)
import Effect (Effect)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

testInput :: String
testInput =
  "1721\n"
    <> "979\n"
    <> "366\n"
    <> "299\n"
    <> "675\n"
    <> "1456"

testData :: Array Int
testData = toIntArray testInput

testFindLuckyPair :: Free TestF Unit
testFindLuckyPair =
  suite "findLuckyPair" do
    test "example pair" do
      equal (Just [ 1721, 299 ]) (findLuckyPair 2020 testData)
    test "non-existant pair" do
      equal (Nothing) (findLuckyPair 2021 testData)

runTests :: Effect Unit
runTests =
  runTest do
    testFindLuckyPair
