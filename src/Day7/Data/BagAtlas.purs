module Day7.Data.BagAtlas where

import Prelude
import Data.Map (Map, empty, foldSubmap, isEmpty, union, unionWith)
import Data.Maybe (Maybe(..))

newtype BagAtlas
  = BagAtlas (Map String (Map String Int))

instance showBagAtlas :: Show BagAtlas where
  show (BagAtlas atlas) =
    foldSubmap
      Nothing
      Nothing
      ( \bagColor containerMap ->
          bagColor
            <> " | "
            <> ( if isEmpty containerMap then
                  "empty"
                else
                  foldSubmap Nothing Nothing (\color count -> color <> ":" <> show count <> " ") containerMap
              )
            <> "\n"
      )
      atlas

instance semigroupBagAtlas :: Semigroup BagAtlas where
  append :: BagAtlas -> BagAtlas -> BagAtlas
  append (BagAtlas a) (BagAtlas b) = BagAtlas $ unionWith union a b

instance monoidBagAtlas :: Monoid BagAtlas where
  mempty :: BagAtlas
  mempty = BagAtlas empty
