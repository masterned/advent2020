module Day10.AdapterArray where

import Prelude
import Data.Int (toNumber)
import Data.List (List(..), drop, filter, foldl, fromFoldable, last, length, snoc, sort, span, union, (:))
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), split, trim)

testInput :: String
testInput = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4\n"

testList :: List Number
testList = numberList testInput

testBag :: List Number
testBag = adapterBag testList

testBag2 :: List Number
testBag2 = adapterBag $ numberList "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"

numberList :: String -> List Number
numberList input = fromFoldable $ (fromMaybe 0.0 <<< fromString) <$> (split (Pattern "\n") (trim input))

joltGapList :: forall a. Ring a => List a -> List a
joltGapList = joltGapList' Nil
  where
  joltGapList' :: List a -> List a -> List a
  joltGapList' acc (first : rest@(second : _)) = joltGapList' (snoc acc (second - first)) rest

  joltGapList' acc _ = acc

-- FIXME: I'm assuming that there's an equation I can substitute
-- FIXME: refactor name
groupPermutationCount :: forall a. List a -> Number
groupPermutationCount list = case length list of
  1 -> 1.0
  2 -> 1.0
  3 -> 2.0
  4 -> 4.0
  _ -> 0.0

-- FIXME: I think this might be where the issue is occurring
reachGroups :: List Number -> List (List Number)
reachGroups = reachGroups' Nil
  where
  reachGroups' :: List (List Number) -> List Number -> List (List Number)
  reachGroups' acc Nil = acc

  reachGroups' acc (hd : tl) =
    (\{ init, rest } -> reachGroups' (union acc ((hd : init) : Nil)) (drop (length init - 1) tl))
      $ span (\num -> num <= (hd + 3.0)) tl

adapterBag :: List Number -> List Number
adapterBag numList = fromMaybe Nil $ snoc (0.0 : sortedList) <$> (add 3.0 <$> (last sortedList))
  where
  sortedList = sort numList

inputPath :: String
inputPath = "./data/Day10/input.txt"

-- NOTE: answer: 3000
getSolutionPart1 :: List Number -> Number
getSolutionPart1 numList = (gapCount 1.0) * (gapCount 3.0)
  where
  joltGaps :: List Number
  joltGaps = joltGapList $ adapterBag numList

  gapCount :: Number -> Number
  gapCount = eq >>> flip filter joltGaps >>> length >>> toNumber

-- NOTE: 2199023255552 too low
-- NOTE: using the other test case, I'm projecting I'm way off
-- NOTE: 20624432955392 is also too low
getSolutionPart2 :: List Number -> String
getSolutionPart2 = adapterBag >>> reachGroups >>> foldl (\product group -> product * (groupPermutationCount group)) 1.0 >>> show

getSolutions :: String -> String
getSolutions input = "Part 1: " <> show (getSolutionPart1 numList) <> "\nPart 2: " <> getSolutionPart2 numList
  where
  numList :: List Number
  numList = numberList input
