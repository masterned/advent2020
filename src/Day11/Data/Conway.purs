module Day11.Data.Conway where

import Prelude
import Data.Array (filter, length, (!!))
import Data.Foldable (sum)
import Data.Input (CharacterTable, toCharacterTable)
import Data.List (List)
import Data.Matrix (Matrix(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Vector (Vector2D(..), vec2)

data CellState
  = Alive
  | Dead
  | Empty

instance showCellState :: Show CellState where
  show :: CellState -> String
  show Alive = "#"
  show Dead = "L"
  show Empty = "."

derive instance eqCellState :: Eq CellState

toCellState :: String -> CellState
toCellState character = case character of
  "L" -> Dead
  "#" -> Alive
  "." -> Empty
  otherwise -> Empty

isAlive :: CellState -> Boolean
isAlive Alive = true

isAlive _ = false

type ConwayMatrix
  = Matrix CellState

toConwayMatrix :: CharacterTable -> ConwayMatrix
toConwayMatrix = map toCellState

getCellState :: ConwayMatrix -> Vector2D -> Maybe CellState
getCellState (Matrix matrix) (Vector2D { x, y }) = (flip (!!) x) =<< (matrix !! y)

countAlive :: ConwayMatrix -> Int
countAlive (Matrix matrix) = sum $ (length <<< filter isAlive) <$> matrix

newtype Neighbourhood
  = Neighbourhood
  { tl :: Maybe CellState
  , tc :: Maybe CellState
  , tr :: Maybe CellState
  , cl :: Maybe CellState
  , self :: CellState
  , cr :: Maybe CellState
  , bl :: Maybe CellState
  , bc :: Maybe CellState
  , br :: Maybe CellState
  }

instance showNeighbourhood :: Show Neighbourhood where
  show :: Neighbourhood -> String
  show (Neighbourhood { tl, tc, tr, cl, self, cr, bl, bc, br }) =
    display tl
      <> " "
      <> display tc
      <> " "
      <> display tr
      <> "\n"
      <> display cl
      <> " "
      <> show self
      <> " "
      <> display cr
      <> "\n"
      <> display bl
      <> " "
      <> display bc
      <> " "
      <> display br
      <> "\n"
    where
    display :: Maybe CellState -> String
    display cell = maybe "?" show cell

getNeighbourhood :: ConwayMatrix -> Vector2D -> Maybe Neighbourhood
getNeighbourhood matrix coord =
  ( \cell ->
      Just
        ( Neighbourhood
            { tl: neighbour (-1) (-1)
            , tc: neighbour 0 (-1)
            , tr: neighbour 1 (-1)
            , cl: neighbour (-1) 0
            , self: cell
            , cr: neighbour 1 0
            , bl: neighbour (-1) 1
            , bc: neighbour 0 1
            , br: neighbour 1 1
            }
        )
  )
    =<< cellAt coord
  where
  cellAt :: Vector2D -> Maybe CellState
  cellAt = getCellState matrix

  neighbour :: Int -> Int -> Maybe CellState
  neighbour xOffset yOffset = cellAt $ coord + vec2 xOffset yOffset

getNeighbours :: Neighbourhood -> Array (Maybe CellState)
getNeighbours (Neighbourhood { tl, tc, tr, cl, cr, bl, bc, br }) = [ tl, tc, tr, cl, cr, bl, bc, br ]

setSelf :: Neighbourhood -> CellState -> Neighbourhood
setSelf (Neighbourhood { tl, tc, tr, cl, cr, bl, bc, br }) self = Neighbourhood { tl, tc, tr, cl, self, cr, bl, bc, br }

type GameRule
  = Neighbourhood -> CellState

type GameRules
  = List GameRule

-- mutate :: GameRules -> Neighbourhood -> CellState
-- mutate rulesList (neighbourhood) = 
noNeighbourBirth :: GameRule
noNeighbourBirth neighbourhood@(Neighbourhood { self }) =
  if self == Empty then
    Empty
  else if isAlive self then
    self
  else if (length $ filter (fromMaybe Empty >>> isAlive) $ getNeighbours neighbourhood) == 0 then
    Alive
  else
    self

fourNeighbourDeath :: GameRule
fourNeighbourDeath neighbourhood@(Neighbourhood { self }) =
  if self == Empty then
    Empty
  else if not isAlive self then
    Dead
  else if (length $ filter (fromMaybe Empty >>> isAlive) $ getNeighbours neighbourhood) >= 4 then
    Dead
  else
    self

age :: GameRules -> ConwayMatrix -> ConwayMatrix
age rulesList matrix = matrix

-- ^ Sandbox
testMatrix :: ConwayMatrix
testMatrix = toConwayMatrix $ toCharacterTable "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"

testNeighbourhood :: Maybe Neighbourhood
testNeighbourhood = getNeighbourhood testMatrix $ vec2 0 0
