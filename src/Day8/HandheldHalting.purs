module Day8.HandheldHalting where

import Prelude
import Data.Either (either, isLeft)
import Data.Foldable (foldl)
import Data.Input (inputNumber)
import Data.List (List)
import Day8.Data.Instruction (flipOperation)
import Day8.Data.Program (Program, modify, parse, collectListOfPossibleBreaks, run)

inputPath :: String
inputPath = inputNumber 8

-- NOTE: answer 1451
getSolutionPart1 :: String -> Int
getSolutionPart1 = parse >>> run >>> either identity identity

-- NOTE: answer 1160
getSolutionPart2 :: String -> Int
getSolutionPart2 input =
  either identity (const (-1))
    $ foldl
        ( \results possibleBreak ->
            if isLeft results then
              results
            else
              run $ modify flipOperation program possibleBreak
        )
        (run program)
        possibleBreaks
  where
  program :: Program
  program = parse input

  possibleBreaks :: List Int
  possibleBreaks = collectListOfPossibleBreaks program

getSolutions :: String -> String
getSolutions input = "Part 1: " <> part1 <> "\nPart 2: " <> part2
  where
  part1 :: String
  part1 = show $ getSolutionPart1 input

  part2 :: String
  part2 = show $ getSolutionPart2 input
