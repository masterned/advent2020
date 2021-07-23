module Day3.Data.Slope where

import Prelude
import Data.Input (CharacterTable)
import Data.Matrix (Matrix, columnCount, getPosition, rowCount)
import Data.Maybe (Maybe)
import Data.Vector (Vector2D)

data SlopePlot
  = Tree
  | Snow

instance showSlopePlot :: Show SlopePlot where
  show :: SlopePlot -> String
  show Tree = "#"
  show Snow = "."

toSlopePlot :: String -> SlopePlot
toSlopePlot character = case character of
  "#" -> Tree
  "." -> Snow
  _ -> Snow

isTree :: SlopePlot -> Boolean
isTree Tree = true

isTree _ = false

type Slope
  = Matrix SlopePlot

toSlope :: CharacterTable -> Slope
toSlope = map toSlopePlot

slopeWidth :: Slope -> Int
slopeWidth = columnCount

slopeHeigth :: Slope -> Int
slopeHeigth = rowCount

getSlopePlot :: Slope -> Vector2D -> Maybe SlopePlot
getSlopePlot = getPosition
