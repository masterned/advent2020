module Day4.Data.Passport where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray, index, (!!))
import Data.Either (hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (ignoreCase, noFlags)
import Record.Builder (buildFromScratch, insert)
import Type.Proxy (Proxy(..))

newtype Year
  = Year Int

instance showYear :: Show Year where
  show :: Year -> String
  show (Year year) = show year

parseYear :: String -> Maybe Year
parseYear = fromString >>> map Year

-- a = (parseYear <=< flip A.index 1) [ (Just "1234") ]
extractYear :: String -> String -> Maybe Year
extractYear regexp =
  flap (match <$> regex regexp ignoreCase)
    >>> hush
    >>> map (map (\nea -> index nea 1 >>= map parseYear))
    >>> join
    >>> join
    >>> join

extractIssueYear :: String -> Maybe Year
extractIssueYear = extractYear "iyr:([0-9]{4})"

extractBirthYear :: String -> Maybe Year
extractBirthYear = extractYear "byr:([0-9]{4})"

extractExpirationYear :: String -> Maybe Year
extractExpirationYear = extractYear "eyr:([0-9]{4})"

newtype Height
  = Height
  { value :: Int
  , unit :: String
  }

instance showHeight :: Show Height where
  show :: Height -> String
  show (Height { value, unit }) = show value <> unit

extractHeight :: String -> Maybe (NonEmptyArray (Maybe String))
extractHeight = join <<< hush <<< flap (match <$> regex "hgt:([0-9]+)(cm|in)" noFlags)

newtype HairColor
  = HairColor String

extractHairColor :: String -> Maybe HairColor
extractHairColor =
  flap (match <$> regex "hcl:(#(?:[0-9a-f]{6}))" ignoreCase)
    >>> hush
    >>> map (map (\nea -> nea !! 1 <#> map HairColor))
    >>> join
    >>> join
    >>> join

instance showHairColor :: Show HairColor where
  show :: HairColor -> String
  show (HairColor hairColor) = hairColor

data EyeColor
  = AMB
  | BLU
  | BRN
  | GRY
  | GRN
  | HZL
  | OTH

extractEyeColor :: String -> Maybe EyeColor
extractEyeColor =
  flap (match <$> regex "pid:([a-z]{3})" ignoreCase)
    >>> map (map (\nea -> nea !! 1))
    >>> hush
    >>> join
    >>> join
    >>> join
    >=> parseEyeColor

parseEyeColor :: String -> Maybe EyeColor
parseEyeColor rawData = case rawData of
  "amb" -> Just AMB
  "blu" -> Just BLU
  "brn" -> Just BRN
  "gry" -> Just GRY
  "grn" -> Just GRN
  "hzl" -> Just HZL
  "oth" -> Just OTH
  _ -> Nothing

instance showEyeColor :: Show EyeColor where
  show :: EyeColor -> String
  show AMB = "amber"
  show BLU = "blue"
  show BRN = "brown"
  show GRY = "grey"
  show GRN = "green"
  show HZL = "hazel"
  show OTH = "other"

newtype ID
  = ID String

instance showID :: Show ID where
  show :: ID -> String
  show (ID id) = id

extractPassportID :: String -> Maybe ID
extractPassportID =
  flap (match <$> regex "pid:([0-9]{9})" noFlags)
    >>> map (map (\nea -> nea !! 1))
    >>> hush
    >>> join
    >>> join
    >>> join
    >>> map ID

extractCountryID :: String -> Maybe ID
extractCountryID =
  flap (match <$> regex "cid:([0-9]{2,3})" noFlags)
    >>> map (map (\nea -> nea !! 1))
    >>> hush
    >>> join
    >>> join
    >>> join
    >>> map ID

newtype Passport
  = Passport
  { byr :: Year
  , iyr :: Year
  , eyr :: Year
  , hgt :: Height
  , hcl :: HairColor
  , ecl :: EyeColor
  , pid :: ID
  , cid :: Maybe ID
  }

instance showPassport :: Show Passport where
  show :: Passport -> String
  show (Passport { byr, iyr, eyr, hgt, ecl, hcl, pid, cid }) =
    "Passport\n[ Birth year: "
      <> show byr
      <> "\n, Issue Year: "
      <> show iyr
      <> "\n, Expiration Year: "
      <> show eyr
      <> "\n, Height: "
      <> show hgt
      <> "\n, Eye Color: "
      <> show ecl
      <> "\n, Hair Color: "
      <> show hcl
      <> "\n, Passport ID: "
      <> show pid
      <> "\n, Country ID: "
      <> show cid
      <> "\n]\n"

parsePassport :: Maybe Passport
parsePassport =
  pure
    $ Passport
    $ buildFromScratch
        ( insert (Proxy :: Proxy "byr") (Year 1995)
            >>> insert (Proxy :: Proxy "iyr") (Year 2020)
            >>> insert (Proxy :: Proxy "eyr") (Year 2030)
            >>> insert (Proxy :: Proxy "hgt") (Height { value: 187, unit: "cm" })
            >>> insert (Proxy :: Proxy "hcl") (HairColor "#887777")
            >>> insert (Proxy :: Proxy "ecl") BRN
            >>> insert (Proxy :: Proxy "pid") (ID "123456789")
            >>> insert (Proxy :: Proxy "cid") Nothing
        )
