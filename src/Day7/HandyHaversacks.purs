module Day7.HandyHaversacks where

import Prelude
import Data.Array.NonEmpty (toArray)
import Data.Either (Either, either)
import Data.Foldable (fold, foldl)
import Data.Int (fromString)
import Data.List (List(..), (:), filter, fromFoldable)
import Data.Map (Map, empty, foldSubmap, insert, isEmpty, keys, lookup, union, unionWith)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split, trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags(..))

testInput :: String
testInput =
  "light red bags contain 1 bright white bag, 2 muted yellow bags.\n"
    <> "dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n"
    <> "bright white bags contain 1 shiny gold bag.\n"
    <> "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n"
    <> "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n"
    <> "dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n"
    <> "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n"
    <> "faded blue bags contain no other bags.\n"
    <> "dotted black bags contain no other bags.\n"

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
  append (BagAtlas a) (BagAtlas b) = BagAtlas (unionWith (union) a b)

instance monoidBagAtlas :: Monoid BagAtlas where
  mempty = BagAtlas empty

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
  ruleLineRegex = regex " bags contain | bags?[,|.]\\s*" (RegexFlags { global: true, unicode: true, ignoreCase: true, sticky: false, multiline: false })

bagSpecRegex :: Either String Regex
bagSpecRegex = regex "^(no other)|(\\d+) (\\w+ \\w+)$" (RegexFlags { global: false, unicode: true, ignoreCase: true, sticky: false, multiline: false })

parseBagSpec :: Array (Maybe String) -> { color :: Maybe String, count :: Maybe Int }
parseBagSpec [ string, nothing, count, color ] =
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
        topBagColor -- key
        (empty) -- value
        empty -- BagAtlas
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
inputPath = "./data/Day7/input.txt"

topBags :: BagAtlas -> String -> Set String
topBags (BagAtlas fullAtlas) bagInQuestion
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

getSolutionPart1 :: Array String -> Int
getSolutionPart1 lines =
  Set.size
    $ topBags
        (fold $ createInvertedBagAtlas <$> separateRuleParts <$> lines)
        "shiny gold"

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
