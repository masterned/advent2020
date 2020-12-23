module Day9.EncodingError where

import Prelude

inputPath :: String
inputPath = "./data/Day9/input.txt"

getSolutionPart1 :: String -> Int
getSolutionPart1 input = -1

getSolutionPart2 :: String -> Int
getSolutionPart2 input = -2

getSolutions :: String -> String
getSolutions input =
  "Part 1: "
    <> (show $ getSolutionPart1 input)
    <> "\nPart 2: "
    <> (show $ getSolutionPart2 input)
