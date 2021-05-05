module Data.Solution where

import Prelude

newtype Solution output
  = Solution
  { part1 :: output
  , part2 :: output
  }

instance showSolution :: (Show output) => Show (Solution output) where
  show :: Show output => Solution output -> String
  show (Solution { part1, part2 }) = "Part 1: " <> (show part1) <> "\nPart 2: " <> (show part2)

getSolutions :: forall input output. (input -> output) -> (input -> output) -> input -> Solution output
getSolutions part1Solution part2Solution inputData = Solution { part1: part1Solution inputData, part2: part2Solution inputData }
