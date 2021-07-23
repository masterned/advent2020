module Day5.BinaryBoarding where

import Prelude
import Data.Array (filter, head, sort)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Input (inputNumber, separateLines)
import Data.Maybe (fromMaybe)
import Data.String (CodePoint, codePointFromChar, toCodePointArray, take, drop)

inputPath :: String
inputPath = inputNumber 5

type Bounds
  = { lower :: Int
    , upper :: Int
    }

lowerHalf :: Bounds -> Bounds
lowerHalf bounds@{ lower, upper }
  | lower >= upper = bounds
  | otherwise = { lower: lower, upper: (lower + upper) / 2 }

upperHalf :: Bounds -> Bounds
upperHalf bounds@{ lower, upper }
  | lower >= upper = bounds
  | otherwise = { lower: (lower + upper) / 2 + 1, upper: upper }

findRow :: Array CodePoint -> Int
findRow =
  result
    <<< foldl
        ( \b cp ->
            if cp == front then
              lowerHalf b
            else if cp == back then
              upperHalf b
            else
              { upper: -1, lower: -1 }
        )
        { lower: 0, upper: 127 }
  where
  result :: Bounds -> Int
  result b = b.lower

  front :: CodePoint
  front = codePointFromChar 'F'

  back :: CodePoint
  back = codePointFromChar 'B'

findCol :: Array CodePoint -> Int
findCol =
  result
    <<< foldl
        ( \b cp ->
            if cp == left then
              lowerHalf b
            else if cp == right then
              upperHalf b
            else
              { upper: -1, lower: -1 }
        )
        { lower: 0, upper: 7 }
  where
  result :: Bounds -> Int
  result b = b.lower

  left :: CodePoint
  left = codePointFromChar 'L'

  right :: CodePoint
  right = codePointFromChar 'R'

findSeat :: String -> Int
findSeat line = row * 8 + col
  where
  rowCode :: Array CodePoint
  rowCode = toCodePointArray $ take 7 line

  colCode :: Array CodePoint
  colCode = toCodePointArray $ drop 7 line

  row :: Int
  row = findRow rowCode

  col :: Int
  col = findCol colCode

-- NOTE: answer 989
getSolutionPart1 :: Array String -> String
getSolutionPart1 = foldl (\greatest cur -> max greatest $ findSeat cur) (-1) >>> show

-- NOTE: answer 548
getSolutionPart2 :: Array String -> String
getSolutionPart2 lines =
  either
    (show)
    (\_ -> "-2")
    $ foldl
        ( \prev cur ->
            ( \val ->
                if val == cur - 1 || val == cur then
                  Right cur
                else
                  Left (cur - 1)
            )
              =<< prev
        )
        (Right (fromMaybe (-1) $ head sortedSeats))
        sortedSeats
  where
  sortedSeats :: Array Int
  sortedSeats = sort $ findSeat <$> (filter (\l -> l /= "") lines)

getSolutions :: String -> String
getSolutions input = "Part 1: " <> getSolutionPart1 inputLines <> "\nPart 2: " <> getSolutionPart2 inputLines
  where
  inputLines :: Array String
  inputLines = separateLines input
