module Data.Input where

import Prelude
import Data.Int as Int
import Data.List (List, fromFoldable)
import Data.Matrix (Matrix(..))
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), singleton, split, toCodePointArray, trim)

separateLines :: String -> Array String
separateLines = trim >>> split (Pattern "\n")

separateParagraphs :: String -> Array String
separateParagraphs = trim >>> split (Pattern "\n\n")

toIntArray :: String -> Array Int
toIntArray = separateLines >>> map (Int.fromString >>> fromMaybe 0)

toNumberList :: String -> List Number
toNumberList = separateLines >>> fromFoldable >>> map (fromString >>> fromMaybe 0.0)

type CharacterTable
  = Matrix String

toCharacterTable :: String -> CharacterTable
toCharacterTable string = Matrix ((separateLines >>> map toCodePointArray >>> map (map singleton)) string)

inputNumber :: Int -> String
inputNumber dayNumber = "./data/Day" <> show dayNumber <> "/input.txt"
