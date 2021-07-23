module Day2.Data.PasswordSpecification where

import Prelude
import Data.Int (fromString)
import Data.String (CodePoint, codePointAt, codePointFromChar)

newtype PasswordSpecification
  = PasswordSpecification
  { min :: Int
  , max :: Int
  , character :: CodePoint
  }

instance semigroupPasswordSpecification :: Semigroup PasswordSpecification where
  append :: PasswordSpecification -> PasswordSpecification -> PasswordSpecification
  append a b = a

instance monoidPasswordSpecification :: Monoid PasswordSpecification where
  mempty :: PasswordSpecification
  mempty = PasswordSpecification { min: 0, max: 0, character: (codePointFromChar ' ') }

setMin :: Int -> PasswordSpecification -> PasswordSpecification
setMin min (PasswordSpecification { min: _, max, character }) = PasswordSpecification { min, max, character }
