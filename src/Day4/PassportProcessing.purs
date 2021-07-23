-- TODO: Write tests
module Day4.PassportProcessing where

import Prelude
import Data.Array (filter, length, (!!), head)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either, either)
import Data.Foldable (all, any, foldl)
import Data.Input (separateParagraphs)
import Data.Int (fromString)
import Data.Map (Map, empty, insert, lookup, member)
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.Regex (Regex, match, regex, test)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags, global, multiline, unicode)

type KeyValuePair
  = { key :: String
    , value :: String
    }

type Passport
  = Map String String

type RawPassportData
  = String

defaultRegexFlags :: RegexFlags
defaultRegexFlags = global <> unicode <> multiline

createKeyValuePair :: String -> KeyValuePair
createKeyValuePair rawKeyValuePair =
  { key: fromMaybe "" $ keyValueArray !! 0
  , value: fromMaybe "" $ keyValueArray !! 1
  }
  where
  keyValueArray :: Array String
  keyValueArray = split (Pattern ":") rawKeyValuePair

parseKeyValuePairSet :: String -> Array KeyValuePair
parseKeyValuePairSet rawData = createKeyValuePair <$> either (pure <<< identity) (flip Regex.split rawData) (regex "\\s+" defaultRegexFlags)

createPassport :: RawPassportData -> Passport
createPassport rawPassportData =
  foldl
    ( \passport { key: key, value: value } ->
        if any (eq key)
          [ "byr"
          , "iyr"
          , "eyr"
          , "hgt"
          , "hcl"
          , "ecl"
          , "pid"
          , "cid"
          ] then
          insert key value passport
        else
          passport
    )
    empty
    kvSet
  where
  kvSet :: Array KeyValuePair
  kvSet = parseKeyValuePairSet rawPassportData

isComplete :: Passport -> Boolean
isComplete passport = all (\key -> member key passport) [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

byrIsValid :: String -> Boolean
byrIsValid birthYear = String.length birthYear == 4 && (maybe false (\byr -> 1920 <= byr && byr <= 2002) $ fromString birthYear)

iyrIsValid :: String -> Boolean
iyrIsValid issueYear = String.length issueYear == 4 && (maybe false (\iyr -> 2010 <= iyr && iyr <= 2020) $ fromString issueYear)

eyrIsValid :: String -> Boolean
eyrIsValid expirationYear = String.length expirationYear == 4 && (maybe false (\eyr -> 2020 <= eyr && eyr <= 2030) $ fromString expirationYear)

hgtIsValid :: String -> Boolean
hgtIsValid height =
  if isCm then
    150 <= internalInt && internalInt <= 193
  else if isIn then
    59 <= internalInt && internalInt <= 76
  else
    false
  where
  testRegex :: String -> Boolean
  testRegex r = either (\_ -> false) (flip test height) (regex r defaultRegexFlags)

  isCm :: Boolean
  isCm = testRegex "^\\d{3}cm$"

  isIn :: Boolean
  isIn = testRegex "^\\d{2}in$"

  internalInt :: Int
  internalInt =
    either
      (const (-1))
      ( \r ->
          fromMaybe (-1)
            $ fromString
            $ fromMaybe ""
            $ head
                ( maybe
                    [ "" ]
                    (\nea -> fromMaybe "" <$> toArray nea)
                    $ match r height
                )
      )
      (regex "\\d+" defaultRegexFlags)

hclIsValid :: String -> Boolean
hclIsValid hairColor = String.length hairColor == 7 && (either (\_ -> false) (flip test hairColor) hairRegex)
  where
  hairRegex :: Either String Regex
  hairRegex = regex "^#[0-9a-f]{6}$" defaultRegexFlags

eclIsValid :: String -> Boolean
eclIsValid eyeColor = any (eq eyeColor) [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

pidIsValid :: String -> Boolean
pidIsValid passportID = String.length passportID == 9 && (isJust $ fromString passportID)

isValid :: Passport -> Boolean
isValid passport =
  isComplete passport
    && (validateWith byrIsValid "byr")
    && (validateWith iyrIsValid "iyr")
    && (validateWith eyrIsValid "eyr")
    && (validateWith hgtIsValid "hgt")
    && (validateWith hclIsValid "hcl")
    && (validateWith eclIsValid "ecl")
    && (validateWith pidIsValid "pid")
  where
  validateWith :: (String -> Boolean) -> String -> Boolean
  validateWith validation key = maybe false validation $ lookup key passport

testSolution :: Int
testSolution = length $ filter isComplete (createPassport <$> testRawPassportData)
  where
  testRawPassportData :: Array String
  testRawPassportData = split (Pattern "\n\n") testInput

inputPath :: String
inputPath = "./data/Day4/input.txt"

-- NOTE: answer 245
getSolutionPart1 :: Array String -> Int
getSolutionPart1 input = length $ filter isComplete passports
  where
  passports :: Array Passport
  passports = createPassport <$> input

-- NOTE: answer 133
getSolutionPart2 :: Array String -> Int
getSolutionPart2 input = length $ filter isValid passports
  where
  passports :: Array Passport
  passports = createPassport <$> input

getSolutions :: String -> String
getSolutions input = "Test: " <> show testSolution <> "\nPart 1: " <> show part1 <> "\nPart 2: " <> show part2
  where
  rawPassportData :: Array String
  rawPassportData = separateParagraphs input

  part1 :: Int
  part1 = getSolutionPart1 rawPassportData

  part2 :: Int
  part2 = getSolutionPart2 rawPassportData

-- ^ Sandbox
testInput :: String
testInput =
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\n"
    <> "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n"
    <> "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\n"
    <> "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
