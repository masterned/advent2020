-- TODO: Write tests
module Day1.ReportRepair where

import Prelude
import Control.MonadZero (guard)
import Data.Array (head)
import Data.Foldable (product)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), split)

inputPath :: String
inputPath = "./data/Day1/input.txt"

findLuckyPair :: Int -> Array Int -> Maybe (Array Int)
findLuckyPair goalSum ns =
  head
    $ do
        i <- ns
        j <- ns
        guard $ i + j == goalSum
        pure [ i, j ]

findLuckyTrio :: Int -> Array Int -> Maybe (Array Int)
findLuckyTrio goalSum ns =
  head
    $ do
        i <- ns
        j <- ns
        k <- ns
        guard $ i + j + k == goalSum
        pure [ i, j, k ]

getSolutionPart1 :: Array Int -> Int
getSolutionPart1 input = product $ fromMaybe [ 0, 0 ] $ findLuckyPair 2020 input

getSolutionPart2 :: Array Int -> Int
getSolutionPart2 input = product $ fromMaybe [ 0, 0, 0 ] $ findLuckyTrio 2020 input

getSolutions :: String -> String
getSolutions input = "Part 1: " <> show part1 <> "\nPart 2: " <> show part2
  where
  legerData :: Array Int
  legerData = (fromMaybe 0 <<< fromString) <$> split (Pattern "\n") input

  part1 :: Int
  part1 = getSolutionPart1 legerData

  part2 :: Int
  part2 = getSolutionPart2 legerData
