-- TODO: Write tests
module Day3.TobogganTrajectory where

import Prelude
import Data.Array (head, length, (!!))
import Data.Foldable (product)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits as CodeUnits

inputPath :: String
inputPath = "./data/Day3/input.txt"

type Slope
  = Array String

type Point
  = { x :: Int
    , y :: Int
    }

type Vector
  = Point

testSlope :: Slope
testSlope =
  [ "..##......."
  , "#...#...#.."
  , ".#....#..#."
  , "..#.#...#.#"
  , ".#...##..#."
  , "..#.##....."
  , ".#.#.#....#"
  , ".#........#"
  , "#.##...#..."
  , "#...##....#"
  , ".#..#...#.#"
  ]

testPosition :: Point
testPosition =
  { x: 0
  , y: 0
  }

testAngle :: Vector
testAngle =
  { x: 3
  , y: 1
  }

isTree :: Char -> Boolean
isTree = eq '#'

getCoord :: Slope -> Point -> Char
getCoord slope position = fromMaybe ' ' $ CodeUnits.charAt position.x $ fromMaybe " " (slope !! position.y)

getRowLength :: Slope -> Int
getRowLength slope = CodeUnits.length $ fromMaybe " " $ head slope

getNextPosition :: Int -> Vector -> Point -> Point
getNextPosition rowLength angle currentPosition =
  { x: mod (currentPosition.x + angle.x) rowLength
  , y: currentPosition.y + angle.y
  }

sled :: Slope -> Vector -> Point -> Int -> Int
sled slope angle currentPosition numHits =
  if currentPosition.y >= length slope then
    numHits
  else if isTree $ getCoord slope currentPosition then
    sled slope angle nextPosition (numHits + 1)
  else
    sled slope angle nextPosition numHits
  where
  slopeWidth :: Int
  slopeWidth = getRowLength slope

  nextPosition :: Point
  nextPosition = getNextPosition slopeWidth angle currentPosition

testSolution :: Int
testSolution = sled testSlope testAngle testPosition 0

getSolutionPart1 :: Array String -> Int
getSolutionPart1 lines = sled lines { x: 3, y: 1 } { x: 0, y: 0 } 0

getSolutionPart2 :: Array String -> Number
getSolutionPart2 lines = product $ (toNumber <<< sledAtAngle) <$> [ { x: 1, y: 1 }, { x: 3, y: 1 }, { x: 5, y: 1 }, { x: 7, y: 1 }, { x: 1, y: 2 } ]
  where
  sledAtAngle :: Vector -> Int
  sledAtAngle angle = sled lines angle { x: 0, y: 0 } 0

getSolutions :: String -> String
getSolutions input = "Part 1: " <> show part1 <> "\nPart 2: " <> show part2
  where
  inputLines :: Array String
  inputLines = split (Pattern "\n") input

  part1 :: Int
  part1 = getSolutionPart1 inputLines

  part2 :: Number
  part2 = getSolutionPart2 inputLines
