module Day8.Data.Program where

import Prelude
import Data.Array (elem, modifyAt, (!!))
import Data.Either (Either(..))
import Data.Foldable (foldl, length)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, trim)
import Day8.Data.Instruction (Instruction(..), empty)
import Day8.Data.Instruction as Instruction
import Day8.Data.Operation (Operation(..))

newtype Program
  = Program (Array Instruction)

instance showProgram :: Show Program where
  show (Program program) = foldl (\string instruction -> string <> show instruction <> "\n") "" program

derive instance eqProgram :: Eq Program

parse :: String -> Program
parse rawProgram =
  Program
    ( Instruction.parse >>> fromMaybe empty
        <$> (split (Pattern "\n") $ trim rawProgram)
    )

run :: Program -> Either Int Int
run = run' 0 0 Nil
  where
  run' :: Int -> Int -> List Int -> Program -> Either Int Int
  run' accumulator instructionLine visitedLineNumbers (Program instructions) =
    if (instructionLine < 0) || (instructionLine > length instructions) || (elem instructionLine visitedLineNumbers) then -- program crashed
      Right accumulator
    else if instructionLine == length instructions then -- program successful
      Left accumulator
    else case operation of
      Jump -> execute accumulator (instructionLine + argument)
      Accumulate -> execute (accumulator + argument) (instructionLine + 1)
      NoOp -> execute accumulator (instructionLine + 1)
    where
    instruction :: Instruction
    instruction = fromMaybe empty $ instructions !! instructionLine

    operation :: Operation
    operation = (\(Instruction { operation: op }) -> op) instruction

    argument :: Int
    argument = (\(Instruction { argument: arg }) -> arg) instruction

    execute :: Int -> Int -> Either Int Int
    execute nextAccumulator nextLineNumber = run' nextAccumulator nextLineNumber (instructionLine : visitedLineNumbers) (Program instructions)

possibleBreakList :: Program -> List Int
possibleBreakList (Program program) = possibleBreakList' Nil 0 Nil (Program program)
  where
  possibleBreakList' :: List Int -> Int -> List Int -> Program -> List Int
  possibleBreakList' possibleBreaks lineNum visited (Program p) =
    if lineNum < 0 || lineNum > length p || elem lineNum visited then
      possibleBreaks
    else if lineNum == length p then
      Nil
    else case operation of
      Jump -> possibleBreakList' (lineNum : possibleBreaks) (lineNum + argument) (lineNum : visited) (Program p)
      NoOp -> possibleBreakList' (lineNum : possibleBreaks) (lineNum + 1) (lineNum : visited) (Program p)
      Accumulate -> possibleBreakList' (possibleBreaks) (lineNum + 1) (lineNum : visited) (Program p)
    where
    instruction :: Instruction
    instruction = fromMaybe empty $ p !! lineNum

    operation :: Operation
    operation = (\(Instruction { operation: op }) -> op) instruction

    argument :: Int
    argument = (\(Instruction { argument: arg }) -> arg) instruction

modify :: (Instruction -> Instruction) -> Program -> Int -> Program
modify modification (Program instructions) lineNumber = Program (fromMaybe [] $ modifyAt lineNumber modification instructions)
