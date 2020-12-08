-- TODO: Write tests
module Day4.PassportProcessing where

import Prelude
import Data.Array (any, filter, find, length, (!!))
import Data.Either (either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String (Pattern(..), split)
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags(..))

type KeyValuePair
  = { key :: String
    , value :: String
    }

type Passport
  = { birthYear :: Int
    , issueYear :: Int
    , expirationYear :: Int
    , height :: String
    , hairColor :: String
    , eyeColor :: String
    , passportID :: Int
    , countryID :: Maybe Int
    }

testInput :: String
testInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

createPassport :: String -> Maybe Passport
createPassport rawPassportData =
  if (any isNothing [ birthYear, issueYear, expirationYear, passportID ]) || (any isNothing [ height, hairColor, eyeColor ]) then
    Nothing
  else
    Just
      { birthYear: fromMaybe (-1) birthYear
      , issueYear: fromMaybe (-1) issueYear
      , expirationYear: fromMaybe (-1) expirationYear
      , height: fromMaybe "" height
      , hairColor: fromMaybe "" hairColor
      , eyeColor: fromMaybe "" eyeColor
      , passportID: fromMaybe (-1) passportID
      , countryID: countryID
      }
  where
  separatePassportParts :: Array String
  separatePassportParts = either (pure <<< identity) (flip Regex.split rawPassportData) (regex "\\s" defaultRegexFlags)

  keyValuePairs :: Array KeyValuePair
  keyValuePairs = createKeyValuePair <$> separatePassportParts

  get :: String -> Maybe String
  get key = (\kv -> kv.value) <$> (find (\kv -> kv.key == key) keyValuePairs)

  birthYear :: Maybe Int
  birthYear = fromString =<< get "byr"

  issueYear :: Maybe Int
  issueYear = fromString =<< get "iyr"

  expirationYear :: Maybe Int
  expirationYear = fromString =<< get "eyr"

  height :: Maybe String
  height = get "hgt"

  hairColor :: Maybe String
  hairColor = get "hcl"

  eyeColor :: Maybe String
  eyeColor = get "ecl"

  passportID :: Maybe Int
  passportID = fromString =<< get "pid"

  countryID :: Maybe Int
  countryID = fromString =<< get "cid"

defaultRegexFlags :: RegexFlags
defaultRegexFlags = RegexFlags { global: true, unicode: true, ignoreCase: false, sticky: false, multiline: true }

createKeyValuePair :: String -> KeyValuePair
createKeyValuePair rawKeyValuePair =
  { key: fromMaybe "" $ keyValueArray !! 0
  , value: fromMaybe "" $ keyValueArray !! 1
  }
  where
  keyValueArray :: Array String
  keyValueArray = split (Pattern ":") rawKeyValuePair

inputPath :: String
inputPath = "./data/Day4/input.txt"

testSolution :: Int
testSolution = length $ filter isJust (createPassport <$> testRawPassportData)
  where
  testRawPassportData :: Array String
  testRawPassportData = split (Pattern "\n\n") testInput

-- NOTE: guessed 203; number too low
-- filtering out too much?
-- NOTE: guessed 263: number too high
-- may have broken my filters?
getSolutionPart1 :: Array String -> Int
getSolutionPart1 input = length $ filter isJust passports
  where
  passports :: Array (Maybe Passport)
  passports = createPassport <$> input

getSolutionPart2 :: Array String -> Int
getSolutionPart2 input = (-2)
  where
  passports :: Array (Maybe Passport)
  passports = createPassport <$> input

getSolutions :: String -> String
getSolutions input = "Test: " <> show testSolution <> "\nPart 1: " <> show part1 <> "\nPart 2: " <> show part2
  where
  rawPassportData :: Array String
  rawPassportData = split (Pattern "\n\n") input

  part1 :: Int
  part1 = getSolutionPart1 rawPassportData

  part2 :: Int
  part2 = getSolutionPart2 rawPassportData
