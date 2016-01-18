module Test.Main where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.Int (even)
import Data.List (List())
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length, charCodeAt, joinWith)
import Data.String.Regex (Regex(), regex, parseFlags, match)
import Data.Tuple (Tuple(..))

import Test.FlareCheck

newtype TRegex = TRegex Regex

instance flammableTRegex :: Flammable TRegex where
  spark = fieldset "Regex" $ TRegex <$>
            (regex <$> string "Pattern" "fo+"
                   <*> (parseFlags <$> string "Flags (g,i,m)" "g"))

newtype Foo = Foo { num :: Number
                  , str :: String
                  , bool :: Boolean
                  , optional :: Maybe Char
                  , arr :: Array Int
                  }

derive instance genericFoo :: Generic Foo

instance interactiveFoo :: Interactive Foo
  where interactive = interactiveGeneric

main = do
  flareCheck' "tests1" "length"      length
  flareCheck' "tests1" "charCodeAt"  charCodeAt
  flareCheck' "tests1" "joinWith"    joinWith
  flareCheck' "tests1" "filter even" (filter even)
  flareCheck' "tests1" "fromMaybe"   (fromMaybe :: Number -> _)

  flareCheck' "tests2" "match" $ \(TRegex regex) string -> match regex string

  let fc :: forall a. Interactive a => String -> a -> _
      fc = flareCheck' "tests3"

  fc "Int" (id :: Int -> _)
  fc "Number" (id :: Number -> _)
  fc "Boolean" (id :: Boolean -> _)
  fc "String" (id :: String -> _)
  fc "Char" (id :: Char -> _)
  fc "Maybe Int" (id :: Maybe Int -> _)
  fc "Maybe String" (id :: Maybe String -> _)
  fc "Either String Int" (id :: Either String Int -> _)
  fc "Tuple Int String" (id :: Tuple Int String -> _)
  fc "Array Int" (id :: Array Int -> _)
  fc "Array String" (id :: Array String -> _)
  fc "List Int" (id :: List Int -> _)
  fc "List String" (id :: List String -> _)
  fc "Nested 1" (Just (Tuple 3 "foo"))
  fc "Nested 2" (Tuple 1 (Tuple 2 (Tuple 3 (Tuple 4 false))))
  fc "Nested Arrays" [[[[1], [2,3]], [[4]]]]
  fc "Records" (Foo { num: 42.3, str: "foo", bool: false, optional: Just '🔥', arr: [2, 17] })
