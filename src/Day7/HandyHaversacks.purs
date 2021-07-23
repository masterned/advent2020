module Day7.HandyHaversacks where

import Prelude
import Data.Array.NonEmpty (toArray)
import Data.Either (Either, either)
import Data.Foldable (fold, foldl)
import Data.Input (inputNumber, separateLines)
import Data.Int (fromString)
import Data.List (List(..), (:), filter, fromFoldable, zip)
import Data.Map (Map, empty, insert, isEmpty, keys, lookup, values)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags, global, unicode, ignoreCase)
import Data.Tuple (Tuple(..))
import Day7.Data.BagAtlas (BagAtlas(..))

separateRuleParts :: String -> List String
separateRuleParts ruleLine =
  either
    (\_ -> "The regex borked..." : Nil)
    identity
    $ filter (not eq "")
    <$> fromFoldable
    <$> ( Regex.split
          <$> ruleLineRegex
          <@> ruleLine
      )
  where
  ruleLineRegex :: Either String Regex
  ruleLineRegex = regex " bags contain | bags?[,|.]\\s*" (global <> unicode <> ignoreCase)

bagSpecRegex :: Either String Regex
bagSpecRegex = regex "^(no other)|(\\d+) (\\w+ \\w+)$" (unicode <> ignoreCase)

parseBagSpec :: Array (Maybe String) -> { color :: Maybe String, count :: Maybe Int }
parseBagSpec [ _, nothing, count, color ] =
  if isJust nothing then
    { color: Nothing, count: Nothing }
  else
    { color: color, count: fromString =<< count }

parseBagSpec _ = { color: Nothing, count: Nothing }

separateBagSpecParts :: String -> { color :: Maybe String, count :: Maybe Int }
separateBagSpecParts bagSpec =
  either
    (\_ -> { color: Nothing, count: Nothing }) -- in case the regex borks
    ( \mNEA ->
        fromMaybe { color: Nothing, count: Nothing }
          (parseBagSpec <$> toArray <$> mNEA)
    )
    (match <$> bagSpecRegex <@> bagSpec)

createBagAtlas :: List String -> BagAtlas
createBagAtlas Nil = BagAtlas empty

createBagAtlas (topBagColor : containedBagsSpecs) =
  BagAtlas
    ( insert
        topBagColor
        ( foldl
            (\bagMap { color, count } -> maybe bagMap (\cl -> insert cl (fromMaybe 0 count) bagMap) color)
            empty
            (separateBagSpecParts <$> containedBagsSpecs)
        )
        empty
    )

createInvertedBagAtlas :: List String -> BagAtlas
createInvertedBagAtlas Nil = BagAtlas empty

createInvertedBagAtlas (topBagColor : containedBagsSpecs) =
  BagAtlas
    ( insert topBagColor empty
        $ foldl
            (\atlas { color, count } -> maybe atlas (\cl -> insert cl (insert topBagColor (fromMaybe 0 count) empty) atlas) color)
            empty
            (separateBagSpecParts <$> containedBagsSpecs)
    )

inputPath :: String
inputPath = inputNumber 7

topBags :: String -> BagAtlas -> Set String
topBags bagInQuestion (BagAtlas fullAtlas)
  | isEmpty fullAtlas = Set.empty
  | otherwise = topBags' bagInQuestion
    where
    topBags' :: String -> Set String
    topBags' bagToFind =
      maybe
        (Set.empty)
        ( \containerBags ->
            foldl
              (\bagSet bagColor -> Set.union bagSet $ topBags' bagColor)
              (Set.fromFoldable $ keys containerBags)
              (keys containerBags)
        )
        (lookup bagToFind fullAtlas)

entries :: forall k v. Ord k => Map k v -> List (Tuple k v)
entries dictionary = zip (fromFoldable $ keys dictionary) (values dictionary)

containedBagsCount :: String -> BagAtlas -> Int
containedBagsCount bagInQuestion (BagAtlas fullAtlas) =
  if isEmpty fullAtlas then
    (-1)
  else
    maybe
      0
      ( foldl
          ( \total (Tuple color count) ->
              total
                + count
                + (count * containedBagsCount color (BagAtlas fullAtlas))
          )
          0
      )
      (entries <$> lookup bagInQuestion fullAtlas)

-- NOTE: answer: 121
part1 :: Array String -> String
part1 =
  map (separateRuleParts >>> createInvertedBagAtlas)
    >>> fold
    >>> topBags "shiny gold"
    >>> Set.size
    >>> show

-- NOTE: answer 3805
part2 :: Array String -> String
part2 =
  map (separateRuleParts >>> createBagAtlas)
    >>> fold
    >>> containedBagsCount "shiny gold"
    >>> show

getSolutions :: String -> String
getSolutions input = "Part 1: " <> part1 lines <> "\nPart 2: " <> part2 lines
  where
  lines :: Array String
  lines = separateLines input

-- ^ Sandbox
