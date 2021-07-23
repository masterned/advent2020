module Day11.SeatingSystem where

import Prelude
import Data.Input (CharacterTable, inputNumber, toCharacterTable)
import Data.List (List(..))
import Data.Matrix (Matrix(..))
import Day11.Data.Conway (ConwayMatrix, GameRules, countAlive, toConwayMatrix)

inputPath :: String
inputPath = inputNumber 11

part1 :: CharacterTable -> String
part1 =
  toConwayMatrix
    >>> mutate (Matrix [ [] ]) Nil
    >>> countAlive
    >>> show
  where
  mutate :: ConwayMatrix -> GameRules -> ConwayMatrix -> ConwayMatrix
  mutate previousState rules nextState = nextState

part2 :: String -> String
part2 input = "-2"

getSolutions :: String -> String
getSolutions input = "Part 1: " <> (part1 $ toCharacterTable input) <> "\nPart 2: " <> part2 input

-- ^: Sandbox
testInput :: String
testInput = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
