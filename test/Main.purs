module Test.Main where

import Prelude

import Data.Maybe
import Data.String
import Data.Tuple
import Data.Tuple.Nested
import Data.String.Regex (Regex(), regex, parseFlags, match)

import Test.FlareCheck

newtype TRegex = TRegex Regex

instance flammableTRegex :: Flammable TRegex where
  spark = fieldset "Regex" $ TRegex <$>
            (regex <$> string "Pattern" "fo+"
                   <*> (parseFlags <$> string "Flags (g,i,m)" "g"))

main = do
  flareCheck' "tests1" "length" length
  flareCheck' "tests1" "charCodeAt" (uncurry charCodeAt)
  flareCheck' "tests1" "fromMaybe" $ uncurry (fromMaybe :: Number -> _)
  flareCheck' "tests2" "match" $ uncurry2 $
    \(TRegex regex) string -> match regex string
