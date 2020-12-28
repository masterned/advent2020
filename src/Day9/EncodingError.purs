module Day9.EncodingError where

import Prelude
import Control.MonadZero (guard)
import Data.Array (drop, null, take, (!!))
import Data.Foldable (length)
import Data.List (List(..), sort, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), split, trim)

inputPath :: String
inputPath = "./data/Day9/input.txt"

-- FIXME: issue may arrise if duplicate numbers appear in sequence
-- FIXME: returns all permutations... only combinations are necessary
--        handle by reference rather than value?
sumPairs :: forall a. Eq a => Semiring a => a -> Array a -> Array (Array a)
sumPairs nextNumber previousNumbers = do
  i <- previousNumbers
  j <- previousNumbers
  guard $ (i /= j) && ((i + j) == nextNumber)
  pure [ i, j ]

containsSumPair :: forall a. Eq a => Semiring a => a -> Array a -> Boolean
containsSumPair nextNumber previousNumbers = not null $ sumPairs nextNumber previousNumbers

findWeakness :: forall a. Eq a => Semiring a => Int -> Array a -> Maybe a
findWeakness _ [] = Nothing

findWeakness window numbers =
  ( \nextNumber ->
      if not containsSumPair nextNumber section then
        Just nextNumber
      else
        findWeakness window (drop 1 numbers)
  )
    =<< (numbers !! window)
  where
  section :: Array a
  section = take window numbers

-- FIXME: breaks on negative numbers...
blackJackCaterpiller :: forall a. Eq a => Ord a => Ring a => a -> List a -> Maybe (List a)
blackJackCaterpiller goal inputNumbers = if List.null collectedHand then Nothing else Just collectedHand
  where
  blackJackCaterpiller' :: a -> List a -> List a -> List a
  blackJackCaterpiller' _ _ Nil = Nil

  blackJackCaterpiller' sumTotal collectedNumbers inputList@(nextInput : restOfInput) =
    if sumTotal == goal then
      collectedNumbers
    else if sumTotal < goal then
      blackJackCaterpiller' (sumTotal + nextInput) (List.Cons nextInput collectedNumbers) (restOfInput)
    else -- sumTotal > goal
      blackJackCaterpiller' (sumTotal - (fromMaybe zero $ List.last collectedNumbers)) (fromMaybe Nil $ List.init collectedNumbers) (inputList)

  collectedHand :: List a
  collectedHand = blackJackCaterpiller' zero Nil inputNumbers

sumMinMax :: forall a. Ord a => Semiring a => List a -> Maybe a
sumMinMax numList
  | length numList <= 1 = Nothing
  | otherwise = add <$> List.head sortedList <*> List.last sortedList
    where
    sortedList :: List a
    sortedList = sort numList

-- NOTE: answer: 731031916
getSolutionPart1 :: Array Number -> String
getSolutionPart1 numberLines = show $ findWeakness 25 numberLines

-- NOTE: answer: 93396727
getSolutionPart2 :: Array Number -> String
getSolutionPart2 numberLines = show $ sumMinMax =<< (blackJackCaterpiller weakness $ List.fromFoldable numberLines)
  where
  weakness :: Number
  weakness = fromMaybe (-1.0) $ findWeakness 25 numberLines

getSolutions :: String -> String
getSolutions input = "Part 1: " <> getSolutionPart1 numberLines <> "\nPart 2: " <> getSolutionPart2 numberLines
  where
  numberLines :: Array Number
  numberLines = fromMaybe (-1.0) <<< fromString <$> (split (Pattern "\n") $ trim input)
