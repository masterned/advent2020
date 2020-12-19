module Day8.HandheldHalting where

import Prelude
import Data.Array ((!!))
import Data.Either (Either, either)
import Data.Foldable (length)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, empty, insert, member)
import Data.String (Pattern(..), split, trim)
import Data.String.Regex (Regex, regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags(..))
import Data.Tuple (Tuple(..))

testInput :: String
testInput =
  "nop +0\n"
    <> "acc +1\n"
    <> "jmp +4\n"
    <> "acc +3\n"
    <> "jmp -3\n"
    <> "acc -99\n"
    <> "acc +1\n"
    <> "jmp -4\n"
    <> "acc +6\n"

parseInstruction :: String -> Maybe (Tuple String Int)
parseInstruction rawInstruction =
  either (\_ -> Nothing)
    ( \pieceArray -> do
        keyword <- pieceArray !! 0
        value <- fromString =<< pieceArray !! 1
        pure (Tuple keyword value)
    )
    instructionPieces
  where
  instructionRegex :: Either String Regex
  instructionRegex = regex " \\+?" (RegexFlags { global: false, sticky: false, ignoreCase: false, multiline: false, unicode: true })

  instructionPieces :: Either String (Array String)
  instructionPieces = Regex.split <$> instructionRegex <@> rawInstruction

runProgram :: Int -> Int -> Set Int -> Array String -> Int
runProgram acc instructionLine visitedLineNumbers instructions =
  if (member instructionLine visitedLineNumbers)
    || (instructionLine >= length instructions)
    || (instructionLine < 0) then
    acc
  else case keyword of
    "jmp" -> runProgram acc (instructionLine + value) (insert instructionLine visitedLineNumbers) instructions
    "acc" -> runProgram (acc + value) (instructionLine + 1) (insert instructionLine visitedLineNumbers) instructions
    "nop" -> runProgram acc (instructionLine + 1) (insert instructionLine visitedLineNumbers) instructions
    _ -> runProgram acc (instructionLine + 1) (insert instructionLine visitedLineNumbers) instructions
  where
  instruction :: Maybe (Tuple String Int)
  instruction = parseInstruction =<< instructions !! instructionLine

  keyword :: String
  keyword = maybe "" (\(Tuple kw _) -> kw) instruction

  value :: Int
  value = maybe 0 (\(Tuple _ v) -> v) instruction

inputPath :: String
inputPath = "./data/Day8/input.txt"

getSolutionPart1 :: Array String -> Int
getSolutionPart1 lines = runProgram 0 0 empty lines

getSolutionPart2 :: Array String -> Int
getSolutionPart2 lines = -2

getSolutions :: String -> String
getSolutions input = "Part 1: " <> part1 <> "\nPart 2: " <> part2
  where
  lines :: Array String
  lines = split (Pattern "\n") $ trim input

  part1 :: String
  part1 = show $ getSolutionPart1 lines

  part2 :: String
  part2 = show $ getSolutionPart2 lines
