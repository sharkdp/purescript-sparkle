module Test.FlareCheck
  ( Flammable
  , spark
  , Read
  , typeName
  , defaults
  , read
  , Interactive
  , createUI
  , flareCheck'
  , flareCheck
  , module Flare
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List(), toList)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.Tuple (Tuple(..))

import Global (readFloat, isFinite)

import Type.Proxy (Proxy(..))

import Signal.Channel (Chan())

import DOM (DOM())
import DOM.Node.Types (Element())

import Signal (runSignal)
import Flare

-- | A type class for input parameters for interactive tests. Instances for
-- | type `a` must provide a way to create a Flare UI which holds a value of
-- | type `a`.
class Flammable a where
  spark :: forall e. UI e a

instance flammableNumber :: Flammable Number where
  spark = number "Number" 3.14

instance flammableInt :: Flammable Int where
  spark = int "Int" 1

instance flammableString :: Flammable String where
  spark = string "String" "foo"

instance flammableBoolean :: Flammable Boolean where
  spark = boolean "Boolean" false

instance flammableTuple :: (Flammable a, Flammable b) => Flammable (Tuple a b) where
  spark = Tuple <$> spark <*> spark

instance flammableMaybe :: (Flammable a) => Flammable (Maybe a) where
  spark = fieldset "Maybe" $ toMaybe <$> boolean "Just" true <*> spark
    where toMaybe true x  = Just x
          toMaybe false _ = Nothing

instance flammableEither :: (Flammable a, Flammable b) => Flammable (Either a b) where
  spark = fieldset "Either" $ toEither <$> radioGroup "Select:" "Left" ["Right"] id <*> spark <*> spark
    where toEither "Left" x _ = Left x
          toEither _      _ y = Right y

-- | A class for types which can be parsed from a `String`. This class is used
-- | to construct input fields for `Array a` and `List a`.
class Read a where
  typeName :: Proxy a -> String
  defaults :: Proxy a -> String
  read :: String -> Maybe a

instance readNumber :: Read Number where
  typeName _ = "Number"
  defaults _ = "0.0,1.1,3.14"
  read str = if isFinite n then (Just n) else Nothing
    where n = readFloat str

instance readInt :: Read Int where
  typeName _ = "Int"
  defaults _ = "0,1,2"
  read = fromString

instance readString :: Read String where
  typeName _ = "String"
  defaults _ = "foo,bar,baz"
  read = Just

instance readBool :: Read Boolean where
  typeName _ = "Boolean"
  defaults _ = "true,false"
  read "true"  = Just true
  read "false" = Just false
  read _       = Nothing

-- | A UI for comma separated values.
csvUI :: forall a e. (Read a) => UI e (Array a)
csvUI = (catMaybes <<< map read <<< split ",") <$> string "CSV:" defaults'
  where defaults' = defaults (Proxy :: Proxy a)

instance flammableArrayRead :: (Read a) => Flammable (Array a) where
  spark = fieldset ("Array " <> typeName') csvUI
    where typeName' = typeName (Proxy :: Proxy a)

instance flammableListRead :: (Read a) => Flammable (List a) where
  spark = fieldset ("List " <> typeName') (toList <$> csvUI)
    where typeName' = typeName (Proxy :: Proxy a)

-- | A type class for interactive tests. Instances must provide a way to create
-- | a Flare UI which returns a `String` as output.
class Interactive t where
  createUI :: forall e. UI e t -> UI e String

-- | A default `createUI` implementation for any `Show`able type.
defaultCreateUI :: forall t e. (Show t) => UI e t -> UI e String
defaultCreateUI = map show

instance interactiveNumber :: Interactive Number where
  createUI = defaultCreateUI

instance interactiveInt :: Interactive Int where
  createUI = defaultCreateUI

instance interactiveString :: Interactive String where
  createUI = defaultCreateUI

instance interactiveBoolean :: Interactive Boolean where
  createUI = defaultCreateUI

instance interactiveMaybe :: (Show a) => Interactive (Maybe a) where
  createUI = defaultCreateUI

instance interactiveEither :: (Show a, Show b) => Interactive (Either a b) where
  createUI = defaultCreateUI

instance interactiveTuple :: (Show a, Show b) => Interactive (Tuple a b) where
  createUI = defaultCreateUI

instance interactiveArray :: (Show a) => Interactive (Array a) where
  createUI = defaultCreateUI

instance interactiveList :: (Show a) => Interactive (List a) where
  createUI = defaultCreateUI

instance interactiveFunction :: (Flammable a, Interactive b) => Interactive (a -> b) where
  createUI f = createUI (f <*> spark)

-- | Append a new interactive test. The arguments are the ID of the parent
-- | element, the title for the test and the list of Flare components. Returns
-- | the element for the output of the test.
foreign import appendTest :: forall e. ElementId
                          -> String
                          -> Array Element
                          -> Eff (dom :: DOM | e) Element

-- | Write the string to the specified output element.
foreign import printOutput :: forall e. Element
                           -> String
                           -> Eff (dom :: DOM | e) Unit

-- | Run an interactive test. The ID specifies the parent element to which
-- | the test will be appended and the label provides a title for the test.
flareCheck' :: forall t e. (Interactive t)
            => ElementId
            -> Label
            -> t
            -> Eff (chan :: Chan, dom :: DOM | e) Unit
flareCheck' parentId title x = do
  let flare = createUI (pure x)
  { components, signal } <- setupFlare flare
  output <- appendTest parentId title components
  runSignal (printOutput output <$> signal)

-- | Run an interactive test. The label provides a title for the test.
flareCheck :: forall t e. (Interactive t)
            => Label
            -> t
            -> Eff (chan :: Chan, dom :: DOM | e) Unit
flareCheck = flareCheck' "tests"
