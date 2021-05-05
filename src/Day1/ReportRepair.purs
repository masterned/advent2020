-- TODO: Write tests
module Day1.ReportRepair where

import Prelude
import Control.MonadZero (guard)
import Data.Array (head)
import Data.Foldable (product)
import Data.Input (inputNumber, toIntArray)
import Data.Maybe (Maybe, maybe)
import Data.Solution (Solution, getSolutions)

inputPath :: String
inputPath = inputNumber 1

findLuckyPair :: forall a. Eq a => Semiring a => a -> Array a -> Maybe (Array a)
findLuckyPair goalSum ns =
  head
    $ do
        i <- ns
        j <- ns
        guard $ i + j == goalSum
        pure [ i, j ]

findLuckyTrio :: forall a. Eq a => Semiring a => a -> Array a -> Maybe (Array a)
findLuckyTrio goalSum ns =
  head
    $ do
        i <- ns
        j <- ns
        k <- ns
        guard $ i + j + k == goalSum
        pure [ i, j, k ]

-- NOTE: answer = 471019
part1 :: Array Int -> Int
part1 = findLuckyPair 2020 >>> map product >>> maybe (-1) identity

-- NOTE: answer = 103927824
part2 :: Array Int -> Int
part2 = findLuckyTrio 2020 >>> map product >>> maybe (-2) identity

answer :: String -> Solution Int
answer = toIntArray >>> getSolutions part1 part2
