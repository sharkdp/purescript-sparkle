module Test.Main where

import Prelude

import Data.Maybe
import Data.String
import Data.String.Regex (Regex(), regex, parseFlags, match)

import Test.FlareCheck

newtype TRegex = TRegex Regex

instance flammableTRegex :: Flammable TRegex where
  spark = fieldset "Regex" $ TRegex <$>
            (regex <$> string "Pattern" "fo+"
                   <*> (parseFlags <$> string "Flags (g,i,m)" "g"))

main = do
  flareCheck' "tests1" "length" length
  flareCheck' "tests1" "charCodeAt" charCodeAt
  flareCheck' "tests1" "fromMaybe" $ fromMaybe :: Number -> _
  flareCheck' "tests2" "match" $ \(TRegex regex) string -> match regex string
