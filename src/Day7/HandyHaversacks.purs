module Day7.HandyHaversacks where

import Prelude
import Data.Array.NonEmpty (toArray)
import Data.Either (Either, either)
import Data.Foldable (foldl, length)
import Data.Int (fromString)
import Data.List (List(..), (:), filter, fromFoldable, head, tail)
import Data.Map (Map, empty, insert, isEmpty, keys, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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

newtype Bag
  = Bag
  { color :: String
  , count :: Int
  }

instance eqBag :: Eq Bag where
  eq (Bag { color: colorA }) (Bag { color: colorB }) = colorA == colorB

derive instance ordBag :: Ord Bag

instance showBag :: Show Bag where
  show (Bag { color, count }) = show color <> ":" <> show count

createBag :: String -> Int -> Bag
createBag color count =
  Bag
    { color: color
    , count: count
    }

getColor :: Bag -> String
getColor (Bag { color }) = color

getCount :: Bag -> Int
getCount (Bag { count }) = count

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

newtype BagAtlas
  = BagAtlas (Map String (List Bag))

derive instance eqBagAtlas :: Eq BagAtlas

instance showBagAtlas :: Show BagAtlas where
  show (BagAtlas atlas) =
    foldl
      ( \output key ->
          output
            <> show key
            <> " | "
            <> ( maybe
                  "shouldn't be able to see this"
                  ( \bagList ->
                      if bagList == Nil then
                        "empty"
                      else
                        foldl (\s b -> s <> show b <> " ") "" bagList
                  )
                  (lookup key atlas)
              )
            <> "\n"
      )
      ""
      $ keys atlas

listJoin :: List String -> String
listJoin = trim <<< foldl (\output s -> output <> s <> " ") ""

createBagAtlas :: List String -> BagAtlas
createBagAtlas Nil = BagAtlas empty

createBagAtlas (topBag : containedBags) =
  BagAtlas
    ( insert topBag Nil
        $ if listJoin containedBags == "no other" then
            empty
          else
            foldl
              ( \atlas bagSpec ->
                  insert (color bagSpec) (createBag topBag (count bagSpec) : Nil) atlas
              )
              empty
              containedBags
    )
  where
  getColorAndCount :: String -> List String
  getColorAndCount bagSpec = fromFoldable $ split (Pattern " ") bagSpec

  color :: String -> String
  color bagSpec = listJoin $ fromMaybe Nil $ tail $ getColorAndCount bagSpec

  count :: String -> Int
  count bagSpec = fromMaybe 0 $ fromString =<< (head $ getColorAndCount bagSpec)

parseBagAtlas :: String -> BagAtlas
parseBagAtlas = separateRuleParts >>> createBagAtlas

mergeBagAtlases :: BagAtlas -> BagAtlas -> BagAtlas
mergeBagAtlases (BagAtlas a) (BagAtlas b)
  | isEmpty a = BagAtlas b
  | isEmpty b = BagAtlas a
  | otherwise = BagAtlas empty

type ConflictResolver
  = Bag -> Bag -> Bag

findPaths :: ConflictResolver -> String -> BagAtlas -> List Bag
findPaths conflictResolver fullAtlas bagInQuestion = Nil

inputPath :: String
inputPath = "./data/Day7/input.txt"

getSolutionPart1 :: Array String -> Int
getSolutionPart1 lines =
  length
    $ findPaths const "shiny gold"
    $ foldl mergeBagAtlases (BagAtlas empty)
    $ parseBagAtlas
    <$> lines

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

-- ^ Experimental
splitBagSpecParts :: String -> Array (Maybe String)
splitBagSpecParts bagSpec =
  either
    (\_ -> [ Just "regex borked" ])
    (\mNEA -> fromMaybe [ Just "" ] $ toArray <$> mNEA)
    $ match
    <$> bagSpecRegex
    <@> bagSpec
  where
  bagSpecRegex :: Either String Regex
  bagSpecRegex = regex "^(no other)|(\\d+) (\\w+ \\w+)$" (RegexFlags { global: false, unicode: true, ignoreCase: true, sticky: false, multiline: false })

bagFromSpec :: Array (Maybe String) -> Bag
bagFromSpec [ _, _, Just countString, Just colorString ] = createBag colorString $ fromMaybe 0 $ fromString countString

bagFromSpec _ = Bag { color: "", count: 0 }
