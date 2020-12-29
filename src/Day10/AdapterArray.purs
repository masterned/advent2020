module Day10.AdapterArray where

import Prelude
import Data.Number (fromString)
import Data.List (List(..), fromFoldable, sort, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, trim)

testInput :: String
testInput = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4\n"

testList :: List Number
testList = fromFoldable $ (fromMaybe 0.0 <<< fromString) <$> (split (Pattern "\n") (trim testInput))

type JoltGapTracker
  = { one :: Number
    , two :: Number
    , three :: Number
    }

getOne :: JoltGapTracker -> Number
getOne { one } = one

incOne :: JoltGapTracker -> JoltGapTracker
incOne jgt = { one: jgt.one + 1.0, two: jgt.two, three: jgt.three }

getTwo :: JoltGapTracker -> Number
getTwo { two } = two

incTwo :: JoltGapTracker -> JoltGapTracker
incTwo jgt = { one: jgt.one, two: jgt.two + 1.0, three: jgt.three }

getThree :: JoltGapTracker -> Number
getThree { three } = three

incThree :: JoltGapTracker -> JoltGapTracker
incThree jgt = { one: jgt.one, two: jgt.two, three: jgt.three + 1.0 }

countJoltGaps :: List Number -> Maybe JoltGapTracker
countJoltGaps Nil = Nothing

countJoltGaps numList = countJoltGaps' { one: 0.0, two: 0.0, three: 0.0 } modifiedList
  where
  modifiedList :: List Number
  modifiedList = 0.0 : (sort numList)

  countJoltGaps' :: JoltGapTracker -> List Number -> Maybe JoltGapTracker
  countJoltGaps' acc Nil = Just acc

  countJoltGaps' acc (_ : Nil) = Just (incThree acc)

  countJoltGaps' acc (first : restOfInput@(second : _)) = case second - first of
    1.0 -> countJoltGaps' (incOne acc) restOfInput
    2.0 -> countJoltGaps' (incTwo acc) restOfInput
    3.0 -> countJoltGaps' (incThree acc) restOfInput
    _ -> Nothing

inputPath :: String
inputPath = "./data/Day10/input.txt"

-- NOTE: answer: 3000
getSolutionPart1 :: List Number -> Number
getSolutionPart1 numberList = (getOne joltGaps) * (getThree joltGaps)
  where
  joltGaps :: JoltGapTracker
  joltGaps = fromMaybe { one: 1.0, two: 0.0, three: -1.0 } $ countJoltGaps numberList

getSolutionPart2 :: List Number -> Number
getSolutionPart2 numberList = -2.0

getSolutions :: String -> String
getSolutions input = "Part 1: " <> show (getSolutionPart1 numberList) <> "\nPart 2: " <> show ""
  where
  numberList :: List Number
  numberList = fromFoldable $ (fromMaybe 0.0 <<< fromString) <$> (split (Pattern "\n") (trim input))
