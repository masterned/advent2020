module Day6.CustomCustoms where

import Prelude
import Data.Foldable (foldl, sum)
import Data.Set (Set, empty, insert, intersection, size, union)
import Data.String (CodePoint, Pattern(..), split, toCodePointArray, trim)

inputPath :: String
inputPath = "./data/Day6/input.txt"

personYeses :: String -> Set CodePoint
personYeses = toCodePointArray >>> (foldl (flip insert) empty)

groupYeses :: String -> Set CodePoint
groupYeses groupString = foldl union empty persons
  where
  persons :: Array (Set CodePoint)
  persons = personYeses <$> split (Pattern "\n") groupString

allChecks :: Set CodePoint
allChecks = foldl (flip insert) empty $ toCodePointArray "abcdefghijklmnopqrstuvwxyz"

unanimousYeses :: String -> Set CodePoint
unanimousYeses groupString = foldl intersection allChecks persons
  where
  persons :: Array (Set CodePoint)
  persons = personYeses <$> split (Pattern "\n") groupString

getSolutionPart1 :: Array String -> Int
getSolutionPart1 customsGroups = sum $ (size <<< groupYeses) <$> customsGroups

getSolutionPart2 :: Array String -> Int
getSolutionPart2 customsGroups = sum $ (size <<< unanimousYeses) <$> customsGroups

getSolutions :: String -> String
getSolutions input = "Part 1: " <> part1 <> "\nPart 2: " <> part2
  where
  customsGroups :: Array String
  customsGroups = split (Pattern "\n\n") $ trim input

  part1 :: String
  part1 = show $ getSolutionPart1 customsGroups

  part2 :: String
  part2 = show $ getSolutionPart2 customsGroups
