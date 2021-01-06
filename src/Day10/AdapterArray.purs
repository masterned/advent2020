module Day10.AdapterArray where

import Prelude
import Data.Foldable (product)
import Data.Int (toNumber)
import Data.List (List(..), filter, foldr, fromFoldable, last, length, snoc, sort, (:))
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), split, trim)

inputPath :: String
inputPath = "./data/Day10/input.txt"

numberList :: String -> List Number
numberList input = fromFoldable $ (fromMaybe 0.0 <<< fromString) <$> (split (Pattern "\n") (trim input))

adapterBag :: List Number -> List Number
adapterBag numList = fromMaybe Nil $ snoc (0.0 : sortedList) <$> (add 3.0 <$> (last sortedList))
  where
  sortedList :: List Number
  sortedList = sort numList

joltGapList :: forall a. Ring a => List a -> List a
joltGapList = joltGapList' Nil
  where
  joltGapList' :: List a -> List a -> List a
  joltGapList' acc (first : rest@(second : _)) = joltGapList' (snoc acc (second - first)) rest

  joltGapList' acc _ = acc

-- based off of Haskell code on:
-- http://vaskir.blogspot.com/2013/09/grouping-consecutive-integers-in-f-and.html
groupConsecutive :: forall a. Eq a => Ring a => List a -> List (List a)
groupConsecutive = foldr group Nil
  where
  group :: a -> List (List a) -> List (List a)
  group x Nil = ((x : Nil) : Nil)

  group x acc@((h : t) : rest)
    | h - x == one = (x : h : t) : rest
    | otherwise = (x : Nil) : acc

  group _ _ = Nil -- this is just to satisfy the type checker, I'm not 100% sure when it would occur

groupPermutations :: List Number -> Number
groupPermutations straight = groupPermutations' $ length straight
  where
  groupPermutations' :: Int -> Number
  groupPermutations' 0 = 0.0

  groupPermutations' 1 = 1.0

  groupPermutations' 2 = 1.0

  groupPermutations' n = groupPermutations' (n - 3) + groupPermutations' (n - 2) + groupPermutations' (n - 1)

-- NOTE: answer == 3000
getSolutionPart1 :: List Number -> Number
getSolutionPart1 numList = (gapCount 1.0) * (gapCount 3.0)
  where
  joltGaps :: List Number
  joltGaps = joltGapList $ adapterBag numList

  gapCount :: Number -> Number
  gapCount = eq >>> flip filter joltGaps >>> length >>> toNumber

-- NOTE: answer == 193434623148032
getSolutionPart2 :: List Number -> String
getSolutionPart2 = adapterBag >>> groupConsecutive >>> map groupPermutations >>> product >>> show

getSolutions :: String -> String
getSolutions input = "Part 1: " <> show (getSolutionPart1 numList) <> "\nPart 2: " <> getSolutionPart2 numList
  where
  numList :: List Number
  numList = numberList input

-- ^: Sandbox
-- Part 2 answer == 8
testList :: List Number
testList = numberList "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4\n"

-- Part 2 answer == 19208
testList2 :: List Number
testList2 = numberList "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"
