module Data.Matrix where

import Prelude
import Data.Array (foldMap, head, length, (!!))
import Data.Maybe (Maybe, maybe)
import Data.Vector (Vector2D(..))

-- TODO: make Foldable instance
newtype Matrix a
  = Matrix (Array (Array a))

instance functorMatrix :: Functor Matrix where
  map :: forall a b. (a -> b) -> Matrix a -> Matrix b
  map fn (Matrix m) = Matrix $ map fn <$> m

instance showMatrix :: Show a => Show (Matrix a) where
  show :: Show a => (Matrix a) -> String
  show (Matrix m) = foldMap showRow m
    where
    showCell :: a -> String
    showCell cell = show cell <> " "

    showRow :: Array a -> String
    showRow row = foldMap showCell row <> "\n"

columnCount :: forall a. Matrix a -> Int
columnCount (Matrix m) = maybe 0 length $ head m

rowCount :: forall a. Matrix a -> Int
rowCount (Matrix m) = length m

getPosition :: forall a. Matrix a -> Vector2D -> Maybe a
getPosition (Matrix slope) (Vector2D { x, y }) = (flip (!!) x) =<< (slope !! y)
