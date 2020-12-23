module Day8.Data.Operation where

import Prelude
import Data.Maybe (Maybe(..))

data Operation
  = NoOp
  | Jump
  | Accumulate

derive instance eqOperation :: Eq Operation

parseOperation :: String -> Maybe Operation
parseOperation rawOperation = case rawOperation of
  "nop" -> Just NoOp
  "jmp" -> Just Jump
  "acc" -> Just Accumulate
  _ -> Nothing
