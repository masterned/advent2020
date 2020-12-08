-- TODO: Write tests
module Day2.PasswordPhilosophy where

import Prelude
import Data.Array (filter, length, (!!))
import Data.Either (either)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), toCodePointArray, split)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (charAt)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags(..))

inputPath :: String
inputPath = "./data/Day2/input.txt"

type Password
  = String

type PasswordSpec
  = { min :: Int
    , max :: Int
    , character :: Char
    , password :: Password
    }

countVerify :: PasswordSpec -> Boolean
countVerify spec = charCount >= spec.min && charCount <= spec.max
  where
  character :: Char
  character = spec.character

  password :: Password
  password = spec.password

  charCount :: Int
  charCount = length $ filter (\cp -> cp == codePointFromChar character) $ toCodePointArray password

positionVerify :: PasswordSpec -> Boolean
positionVerify spec =
  ((getChar firstPosition == character) && (getChar secondPosition /= character))
    || ((getChar firstPosition /= character) && (getChar secondPosition == character))
  where
  character :: Char
  character = spec.character

  password :: Password
  password = spec.password

  firstPosition :: Int
  firstPosition = spec.min - 1

  secondPosition :: Int
  secondPosition = spec.max - 1

  getChar :: Int -> Char
  getChar position = fromMaybe ' ' $ charAt position password

toPasswordSpec :: String -> PasswordSpec
toPasswordSpec line =
  { min: intFromPart 0
  , max: intFromPart 1
  , character: fromMaybe ' ' (charAt 0 $ fromMaybe " " $ parts !! 2)
  , password: fromMaybe " " $ parts !! 3
  }
  where
  separateParts :: String -> Array String
  separateParts l = either (pure <<< identity) (flip Regex.split l) (Regex.regex " |: |-" defaultRegexFlags)

  parts :: Array String
  parts = separateParts line

  intFromPart :: Int -> Int
  intFromPart index = fromMaybe (-1) $ fromString $ fromMaybe " " $ parts !! index

defaultRegexFlags :: RegexFlags
defaultRegexFlags = RegexFlags { global: true, ignoreCase: true, multiline: false, sticky: false, unicode: true }

getSolutionPart1 :: Array String -> Int
getSolutionPart1 = length <<< filter (countVerify <<< toPasswordSpec)

getSolutionPart2 :: Array String -> Int
getSolutionPart2 = length <<< filter (positionVerify <<< toPasswordSpec)

getSolutions :: String -> String
getSolutions input = "Part 1: " <> show part1 <> "\nPart 2: " <> show part2
  where
  inputLines :: Array String
  inputLines = split (Pattern "\n") input

  part1 :: Int
  part1 = getSolutionPart1 inputLines

  part2 :: Int
  part2 = getSolutionPart2 inputLines
