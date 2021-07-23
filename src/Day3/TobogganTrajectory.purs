module Day3.TobogganTrajectory where

import Prelude
import Data.Foldable (product)
import Data.Input (inputNumber, toCharacterTable)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Solution (Solution, getSolutions)
import Data.Vector (Vector2D(..), getY, vec2)
import Day3.Data.Slope (Slope, getSlopePlot, isTree, slopeHeigth, slopeWidth, toSlope)

inputPath :: String
inputPath = inputNumber 3

getNextPosition :: Int -> Vector2D -> Vector2D -> Vector2D
getNextPosition rowLength (Vector2D angle) (Vector2D currentPosition) =
  vec2
    (mod (currentPosition.x + angle.x) rowLength)
    (currentPosition.y + angle.y)

sled :: Vector2D -> Slope -> Int
sled = sled' 0 zero
  where
  sled' :: Int -> Vector2D -> Vector2D -> Slope -> Int
  sled' numHits currentPosition angle slope =
    if getY currentPosition >= slopeHeigth slope then
      numHits
    else if maybe false isTree $ getSlopePlot slope currentPosition then
      sled' (numHits + 1) nextPosition angle slope
    else
      sled' numHits nextPosition angle slope
    where
    nextPosition :: Vector2D
    nextPosition = getNextPosition (slopeWidth slope) angle currentPosition

multisled :: Array Vector2D -> Slope -> Array Int
multisled = map sled >>> flap

-- NOTE: answer = 184
part1 :: Slope -> Number
part1 = sled (vec2 3 1) >>> toNumber

-- NOTE: answer = 2431272960
part2 :: Slope -> Number
part2 = multisled [ vec2 1 1, vec2 3 1, vec2 5 1, vec2 7 1, vec2 1 2 ] >>> map toNumber >>> product

answer :: String -> Solution Number
answer = toCharacterTable >>> toSlope >>> getSolutions part1 part2
