module Day3.Test.TobogganTrajectory where

import Prelude
import Control.Monad.Free (Free)
import Data.Input (toCharacterTable)
import Data.Vector (vec2)
import Day3.Data.Slope (Slope, toSlope)
import Day3.TobogganTrajectory (getNextPosition, multisled, sled)
import Effect (Effect)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

testInput :: String
testInput =
  "..##.......\n"
    <> "#...#...#..\n"
    <> ".#....#..#.\n"
    <> "..#.#...#.#\n"
    <> ".#...##..#.\n"
    <> "..#.##.....\n"
    <> ".#.#.#....#\n"
    <> ".#........#\n"
    <> "#.##...#...\n"
    <> "#...##....#\n"
    <> ".#..#...#.#"

testSlope :: Slope
testSlope = toSlope $ toCharacterTable testInput

testGetNextPosition :: Free TestF Unit
testGetNextPosition =
  suite "getNextPosition" do
    test "basic positive single row" do
      equal (vec2 1 1) (getNextPosition 10 (vec2 0 0) (vec2 1 1))
    test "positive wrap single row" do
      equal (vec2 0 1) (getNextPosition 3 (vec2 2 0) (vec2 1 1))

testSled :: Free TestF Unit
testSled =
  suite "sled (fn)" do
    test "example version" do
      equal 7 (sled (vec2 3 1) testSlope)

testMultisled :: Free TestF Unit
testMultisled =
  suite "multisled" do
    test "example version" do
      equal [ 2, 7, 3, 4, 2 ] (multisled [ vec2 1 1, vec2 3 1, vec2 5 1, vec2 7 1, vec2 1 2 ] testSlope)

runTests :: Effect Unit
runTests =
  runTest do
    testGetNextPosition
    testSled
    testMultisled
