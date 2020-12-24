module Day9.EncodingError where

import Prelude
import Control.MonadZero (guard)
import Data.Array (drop, head, last, take, (!!), (:))
import Data.Foldable (foldr, length, sum)
import Data.Maybe (Maybe, fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), split, trim)

inputPath :: String
inputPath = "./data/Day9/input.txt"

containsSumPair :: Number -> Array Number -> Boolean
containsSumPair nextNumber previousNumbers =
  1
    <= ( length
          $ do
              a <- previousNumbers
              b <- previousNumbers
              guard $ (a /= b) && (a + b == nextNumber)
              pure [ a, b ]
      )

findWeakness :: Array Number -> Number
findWeakness numbers =
  if not containsSumPair nextNumber section then
    nextNumber
  else
    findWeakness (drop 1 numbers)
  where
  section :: Array Number
  section = take 25 numbers

  nextNumber :: Number
  nextNumber = fromMaybe 0.0 $ numbers !! 25

blackJack :: Array Number -> Number -> Array Number
blackJack [] _ = [ -1.0 ]

blackJack input weakness =
  if (sum hit) == weakness then
    hit
  else
    blackJack (drop 1 input) weakness
  where
  hit :: Array Number
  hit =
    foldr
      ( \n hand ->
          if sum hand < weakness then
            n : hand
          else
            hand
      )
      []
      input

sumTopAndBottom :: Array Number -> Maybe Number
sumTopAndBottom array = ((+) <$> head array <*> last array)

-- 731031916
getSolutionPart1 :: Array Number -> String
getSolutionPart1 numberLines = show $ findWeakness numberLines

getSolutionPart2 :: Array Number -> String
getSolutionPart2 numberLines = show $ blackJack numberLines weakness
  where
  weakness :: Number
  weakness = findWeakness numberLines

getSolutions :: String -> String
getSolutions input = "Part 1: " <> getSolutionPart1 numberLines <> "\nPart 2: " <> getSolutionPart2 numberLines
  where
  numberLines :: Array Number
  numberLines = fromMaybe (-1.0) <<< fromString <$> (split (Pattern "\n") $ trim input)
