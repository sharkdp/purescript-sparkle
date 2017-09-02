module Test.Main where

import Prelude

import Data.Array (partition)
import Data.Either (Either, fromRight)
import Data.Enum (class Enum, class BoundedEnum, defaultSucc, defaultPred, Cardinality(..))
import Data.Generic (class Generic, gShow)
import Data.Int (even)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.String (length, charCodeAt, joinWith)
import Data.String.Regex (Regex(), regex, parseFlags, match)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Color (Color, mix, ColorSpace(..))

import Flare (fieldset, string)
import Sparkle (class Flammable, class Interactive, sparkle', NonNegativeInt(..),
                SmallInt(..), SmallNumber(..), Multiline(..), WrapEnum, sparkleDoc')

newtype TRegex = TRegex Regex

instance flammableTRegex ∷ Flammable TRegex where
  examples = TRegex (unsafePartial $ fromRight (regex "fo+" (parseFlags "g"))) :| Nil
  spark _ = fieldset "Regex" $ TRegex <$>
              (regex' <$> string "Pattern" "fo+"
                      <*> (parseFlags <$> string "Flags (g,i,m)" "g"))
    where regex' pattern flags = unsafePartial $ fromRight (regex pattern flags)

type Foo = { num ∷ Number
           , str ∷ String
           , bool ∷ Boolean
           , optional ∷ Maybe Char
           , arr ∷ Array Int
           }

data TestEnum = Option1 | Option2 | TrueOrFalse Boolean

derive instance genericTestEnum ∷ Generic TestEnum

instance showTestEnum ∷ Show TestEnum where
  show = gShow

testFromEnum Option1 = 0
testFromEnum Option2 = 1
testFromEnum (TrueOrFalse false) = 2
testFromEnum (TrueOrFalse true) = 3

testToEnum 0 = Just Option1
testToEnum 1 = Just Option2
testToEnum 2 = Just $ TrueOrFalse false
testToEnum 3 = Just $ TrueOrFalse true
testToEnum _ = Nothing

derive instance eqTestEnum ∷ Eq TestEnum
derive instance ordTestEnum ∷ Ord TestEnum

instance boundedTestEnum ∷ Bounded TestEnum where
  bottom = Option1
  top = TrueOrFalse true

instance enumTestEnum ∷ Enum TestEnum where
  succ = defaultSucc testToEnum testFromEnum
  pred = defaultPred testToEnum testFromEnum

instance boundedEnumTestEnum ∷ BoundedEnum TestEnum where
  cardinality = Cardinality 4

  fromEnum = testFromEnum
  toEnum = testToEnum

main = do
  sparkle' "tests1" "length"         length
  sparkle' "tests1" "charCodeAt"     charCodeAt
  sparkle' "tests1" "joinWith"       joinWith
  sparkle' "tests1" "partition even" (partition even)

  sparkle' "tests2" "match" $ \(TRegex regex) string → match regex string

  let mixHSL = mix HSL
  sparkleDoc' "testsDoc" "mixHSL :: Color -> Color -> Number -> Color"
    (Just $ "Mix two colors by linearly interpolating between them in the HSL color space. " <>
            "The shortest path is chosen along the circle of hue values.")
    (\c1 c2 (SmallNumber n) -> mixHSL c1 c2 n)

  let fc ∷ ∀ a. Interactive a => String → a → _
      fc = sparkle' "tests3"

  fc "Int" (id ∷ Int → _)
  fc "Number" (id ∷ Number → _)
  fc "Boolean" (id ∷ Boolean → _)
  fc "String" (id ∷ String → _)
  fc "Char" (id ∷ Char → _)
  fc "Maybe Int" (id ∷ Maybe Int → _)
  fc "Maybe String" (id ∷ Maybe String → _)
  fc "Either String Int" (id ∷ Either String Int → _)
  fc "Tuple Int String" (id ∷ Tuple Int String → _)
  fc "Array Int" (id ∷ Array Int → _)
  fc "Array String" (id ∷ Array String → _)
  fc "List SmallNumber" (id ∷ List SmallNumber → _)
  fc "List String" (id ∷ List String → _)
  fc "Color" (id ∷ Color → _)
  fc "Record" (id ∷ Foo → _)
  fc "Nested 1" (Just (Tuple 3 "foo"))
  fc "Nested 2" (Tuple 1 (Tuple 2 (Tuple 3 (Tuple 4 false))))
  fc "Nested Arrays" [[[[1], [2,3]], [[4]]]]
  fc "Records" ({ num: 42.3, str: "foo", bool: false, optional: Just '☀', arr: [2, 17] })
  fc "NonNegativeInt" (\(NonNegativeInt x) → x)
  fc "SmallInt" (\(SmallInt x) → x)
  fc "SmallNumber" (\(SmallNumber x) → x)
  fc "Multiline" (\(Multiline str) → str)
  fc "WrapEnum" (id ∷ (WrapEnum TestEnum) → _)
