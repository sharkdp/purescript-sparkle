module Test.FlareCheck
  ( Flammable
  , spark
  , Read
  , typeName
  , defaults
  , read
  , Interactive
  , createUI
  , Renderable()
  , flareCheck'
  , flareCheck
  , module Flare
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Array as A
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List(), toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split, length, charAt)
import Data.Tuple (Tuple(..))

import Global (readFloat, isFinite)

import Type.Proxy (Proxy(..))

import Signal.Channel (Chan())

import DOM (DOM())
import DOM.Node.Types (Element())

import Text.Smolder.Markup as H
import Text.Smolder.Markup ((!))
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Renderer.String as H

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

instance flammableChar :: Flammable Char where
  spark = fromMaybe ' ' <$> charAt 0 <$> stringPattern "Char" "^.$" "f"

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

instance readChar :: Read Char where
  typeName _ = "Char"
  defaults _ = "f,o,o"
  read = charAt 0

instance readBool :: Read Boolean where
  typeName _ = "Boolean"
  defaults _ = "true,false"
  read "true"  = Just true
  read "false" = Just false
  read _       = Nothing

-- | A UI for comma separated values.
csvUI :: forall a e. (Read a) => UI e (Array a)
csvUI = (A.catMaybes <<< map read <<< split ",") <$> string "CSV:" defaults'
  where defaults' = defaults (Proxy :: Proxy a)

instance flammableArrayRead :: (Read a) => Flammable (Array a) where
  spark = fieldset ("Array " <> typeName') csvUI
    where typeName' = typeName (Proxy :: Proxy a)

instance flammableListRead :: (Read a) => Flammable (List a) where
  spark = fieldset ("List " <> typeName') (toList <$> csvUI)
    where typeName' = typeName (Proxy :: Proxy a)

-- | A data type that describes possible output actions and values for an
-- | interactive test.
data Renderable
  = SetText String
  | SetHTML H.Markup

-- | A type class for interactive tests. Instances must provide a way to create
-- | a Flare UI which returns a `String` as output.
class Interactive t where
  createUI :: forall e. UI e t -> UI e Renderable

-- | A default `createUI` implementation for any `Show`able type.
defaultCreateUI :: forall t e. (Show t) => UI e t -> UI e Renderable
defaultCreateUI = map (SetText <<< show)

instance interactiveNumber :: Interactive Number where
  createUI = defaultCreateUI

instance interactiveInt :: Interactive Int where
  createUI = defaultCreateUI

instance interactiveString :: Interactive String where
  createUI = map (SetHTML <<< pretty)
    where
      pretty val = do H.pre $
                        H.span ! HA.className "flarecheck-string" $ H.text (show val)
                      H.text ("String length: " <> show (length val))

instance interactiveChar :: Interactive Char where
  createUI = defaultCreateUI

instance interactiveBoolean :: Interactive Boolean where
  createUI = map (SetHTML <<< pretty)
    where
      pretty true =  H.pre ! HA.className "flarecheck-okay" $
                       H.b (H.text "true")
      pretty false = H.pre ! HA.className "flarecheck-warn" $
                       H.b (H.text "false")

instance interactiveOrdering :: Interactive Ordering where
  createUI = defaultCreateUI

instance interactiveMaybe :: (Show a) => Interactive (Maybe a) where
  createUI = map (SetHTML <<< pretty)
    where
      pretty Nothing  = H.pre ! HA.className "flarecheck-warn" $
                          H.b (H.text "Nothing")
      pretty (Just v) = H.pre ! HA.className "flarecheck-okay" $ do
                          H.b (H.text "Just")
                          H.text (" (" <> show v <> ")")


instance interactiveEither :: (Show a, Show b) => Interactive (Either a b) where
  createUI = map (SetHTML <<< pretty)
    where
      pretty (Left v)  = H.pre ! HA.className "flarecheck-warn" $ do
                           H.b (H.text "Left")
                           H.text (" (" <> show v <> ")")
      pretty (Right v) = H.pre ! HA.className "flarecheck-okay" $ do
                           H.b (H.text "Right")
                           H.text (" (" <> show v <> ")")

instance interactiveTuple :: (Show a, Show b) => Interactive (Tuple a b) where
  createUI = defaultCreateUI

instance interactiveArray :: (Show a) => Interactive (Array a) where
  createUI = map (SetHTML <<< pretty)
    where
      pretty val = do H.pre $ H.text (show val)
                      H.text ("Array length: " <> show (A.length val))

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
foreign import setText :: forall e. Element
                       -> String
                       -> Eff (dom :: DOM | e) Unit

-- | Set innerHTML of the specified output element.
foreign import setHTML :: forall e. Element
                       -> String
                       -> Eff (dom :: DOM | e) Unit

render :: forall e. Element
       -> Renderable
       -> Eff (dom :: DOM | e) Unit
render output (SetText str) = setText output str
render output (SetHTML markup) = setHTML output (H.render markup)

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
  runSignal (render output <$> signal)

-- | Run an interactive test. The label provides a title for the test.
flareCheck :: forall t e. (Interactive t)
            => Label
            -> t
            -> Eff (chan :: Chan, dom :: DOM | e) Unit
flareCheck = flareCheck' "tests"
