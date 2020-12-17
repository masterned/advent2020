module Day7.HandyHaversacks where

import Prelude
import Data.Array.NonEmpty (toArray)
import Data.Either (Either, either)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List (List(..), (:), filter, fromFoldable)
import Data.Map (Map, empty, foldSubmap, insert, isEmpty, keys, lookup, member, size, union, unionWith)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
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

derive instance eqBagAtlas :: Eq BagAtlas

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

createBagAtlas :: List String -> BagAtlas
createBagAtlas Nil = BagAtlas empty

createBagAtlas (topBagColor : containedBagsSpecs) =
  BagAtlas
    ( insert topBagColor empty
        $ foldl
            (\atlas { color, count } -> maybe atlas (\cl -> insert cl (insert topBagColor (fromMaybe 0 count) empty) atlas) color)
            empty
            (splitBagSpecParts <$> containedBagsSpecs)
    )

parseBagAtlas :: String -> BagAtlas
parseBagAtlas = separateRuleParts >>> createBagAtlas

mergeBagAtlases :: BagAtlas -> BagAtlas -> BagAtlas
mergeBagAtlases (BagAtlas a) (BagAtlas b) = BagAtlas (unionWith (union) a b)

inputPath :: String
inputPath = "./data/Day7/input.txt"

findPaths :: (Int -> Int -> Int) -> String -> BagAtlas -> Map String Int
findPaths conflictResolver bagInQuestion (BagAtlas fullAtlas)
  | member bagInQuestion fullAtlas =
    maybe
      empty
      ( \containerBags ->
          if isEmpty containerBags then
            empty
          else
            foldl (\bagList bagColor -> unionWith conflictResolver bagList $ findPaths conflictResolver bagColor (BagAtlas fullAtlas)) containerBags
              $ keys containerBags
      )
      $ lookup bagInQuestion fullAtlas
  | otherwise = empty

getSolutionPart1 :: Array String -> Int
getSolutionPart1 lines =
  size
    $ findPaths const "shiny gold"
    $ foldl mergeBagAtlases (BagAtlas empty)
    $ (parseBagAtlas <$> lines)

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

splitBagSpecParts :: String -> { color :: Maybe String, count :: Maybe Int }
splitBagSpecParts bagSpec =
  either
    (\_ -> { color: Nothing, count: Nothing }) -- in case the regex borks
    ( \mNEA ->
        fromMaybe { color: Nothing, count: Nothing }
          (parseBagSpec <$> toArray <$> mNEA)
    )
    (match <$> bagSpecRegex <@> bagSpec)
  where
  bagSpecRegex :: Either String Regex
  bagSpecRegex = regex "^(no other)|(\\d+) (\\w+ \\w+)$" (RegexFlags { global: false, unicode: true, ignoreCase: true, sticky: false, multiline: false })

  parseBagSpec :: Array (Maybe String) -> { color :: Maybe String, count :: Maybe Int }
  parseBagSpec [ string, nothing, count, color ] =
    if isJust nothing then
      { color: Nothing, count: Nothing }
    else
      { color: color, count: fromString =<< count }

  parseBagSpec _ = { color: Nothing, count: Nothing }
