module Day8.Data.Instruction where

import Prelude
import Data.Array ((!!))
import Data.Either (Either, either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, regex, split)
import Data.String.Regex.Flags (unicode)
import Day8.Data.Operation (Operation(..), parseOperation)

newtype Instruction
  = Instruction
  { operation :: Operation
  , argument :: Int
  }

instance showInstruction :: Show Instruction where
  show (Instruction { operation, argument }) =
    ( case operation of
        NoOp -> "nop"
        Jump -> "jmp"
        Accumulate -> "acc"
    )
      <> " "
      <> ( if argument < 0 then
            show argument
          else
            "+" <> show argument
        )

derive instance eqInstruction :: Eq Instruction

parse :: String -> Maybe Instruction
parse rawInstruction =
  either (\_ -> Nothing)
    ( \pieceArray -> do
        operation <- parseOperation =<< pieceArray !! 0
        argument <- fromString =<< pieceArray !! 1
        pure (Instruction { operation: operation, argument: argument })
    )
    (split <$> instructionRegex <@> rawInstruction)
  where
  instructionRegex :: Either String Regex
  instructionRegex = regex " \\+?" unicode

empty :: Instruction
empty = (Instruction { operation: NoOp, argument: 0 })

getOperation :: Instruction -> Operation
getOperation (Instruction { operation }) = operation

setOperation :: Operation -> Instruction -> Instruction
setOperation operation (Instruction { argument }) = Instruction { operation, argument }

flipOperation :: Instruction -> Instruction
flipOperation instruction = case getOperation instruction of
  Jump -> setOperation NoOp instruction
  NoOp -> setOperation Jump instruction
  _ -> instruction
