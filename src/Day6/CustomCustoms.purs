module Day6.CustomCustoms where

import Prelude
import Data.Foldable (foldl, sum)
import Data.Input (inputNumber, separateLines, separateParagraphs)
import Data.Set (Set, empty, insert, intersection, size, union)
import Data.String (CodePoint, toCodePointArray)

inputPath :: String
inputPath = inputNumber 6

personYeses :: String -> Array (Set CodePoint)
personYeses = separateLines >>> map (toCodePointArray >>> (foldl (flip insert) empty))

groupYeses :: Array (Set CodePoint) -> Set CodePoint
groupYeses = foldl union empty

unanimousYeses :: Array (Set CodePoint) -> Set CodePoint
unanimousYeses = foldl intersection allChecks
  where
  allChecks :: Set CodePoint
  allChecks = foldl (flip insert) empty $ toCodePointArray "abcdefghijklmnopqrstuvwxyz"

countYeses :: (Array (Set CodePoint) -> Set CodePoint) -> Array String -> Int
countYeses countingFn = map (personYeses >>> countingFn >>> size) >>> sum

-- NOTE: answer 6703
part1 :: Array String -> String
part1 = countYeses groupYeses >>> show >>> append "Part 1: "

-- NOTE: answer 3430
part2 :: Array String -> String
part2 = countYeses unanimousYeses >>> show >>> append "Part 2: "

getSolutions :: String -> String
getSolutions input = part1 customsGroups <> "\n" <> part2 customsGroups
  where
  customsGroups :: Array String
  customsGroups = separateParagraphs input
