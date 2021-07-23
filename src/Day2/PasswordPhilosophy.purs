-- TODO: Write tests
module Day2.PasswordPhilosophy where

import Prelude
import Data.Array (filter, length, (!!))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either, either)
import Data.Input (separateLines)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Solution (Solution, getSolutions)
import Data.String (toCodePointArray)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (charAt)
import Data.String.Regex (match, regex, split)
import Data.String.Regex.Flags (global, noFlags)

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

separateParts :: String -> Array String
separateParts l = either (pure <<< identity) (flip split l) (regex " |: |-" global)

breakApart :: String -> Either String (Maybe (NonEmptyArray (Maybe String)))
breakApart = flap $ match <$> regex "^([0-9]+)-([0-9]+)\\s+(.):\\s+(.*)$" noFlags

toPasswordSpec :: String -> PasswordSpec
toPasswordSpec line =
  { min: intFromPart 0
  , max: intFromPart 1
  , character: fromMaybe ' ' $ charAt 0 =<< parts !! 2
  , password: fromMaybe " " $ parts !! 3
  }
  where
  parts :: Array String
  parts = separateParts line

  intFromPart :: Int -> Int
  intFromPart index = fromMaybe (-1) $ fromString =<< (parts !! index)

countVerify :: PasswordSpec -> Boolean
countVerify { min, max, character, password } = charCount >= min && charCount <= max
  where
  charCount :: Int
  charCount = length $ filter (eq $ codePointFromChar character) $ toCodePointArray password

positionVerify :: PasswordSpec -> Boolean
positionVerify { min, max, character, password } =
  (isChar firstPosition && not isChar secondPosition)
    || (not isChar firstPosition && isChar secondPosition)
  where
  firstPosition :: Int
  firstPosition = min - 1

  secondPosition :: Int
  secondPosition = max - 1

  isChar :: Int -> Boolean
  isChar position = maybe false (eq character) $ charAt position password

countValidPasswords :: (PasswordSpec -> Boolean) -> Array String -> Int
countValidPasswords verification = filter (toPasswordSpec >>> verification) >>> length

-- NOTE: answer = 620
part1 :: Array String -> Int
part1 = countValidPasswords countVerify

-- NOTE: answer = 727
part2 :: Array String -> Int
part2 = countValidPasswords positionVerify

answer :: String -> Solution Int
answer = separateLines >>> getSolutions part1 part2
