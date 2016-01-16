module Test.FlareCheck
  ( class Flammable
  , spark
  , class Read
  , typeName
  , defaults
  , read
  , class Interactive
  , createUI
  , showCreateUI
  , foldableCreateUI
  , gCreateUI
  , Renderable()
  , flareDoc'
  , flareDoc
  , flareCheck'
  , flareCheck
  , module Flare
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, for_, intercalate, foldr)
import Data.Generic (class Generic, GenericSpine(..), toSpine)
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
  spark = fieldset "Tuple" $ Tuple <$> spark <*> spark

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
showCreateUI :: forall t e. (Show t) => UI e t -> UI e Renderable
showCreateUI = map (SetText <<< show)

-- | A default `createUI` implementation for `Foldable` types.
foldableCreateUI :: forall f a e. (Foldable f, Show a) => UI e (f a) -> UI e Renderable
foldableCreateUI = map (SetHTML <<< pretty)
  where
    pretty val | null val =
      H.table $ H.tr $ H.td $
        H.pre ! HA.className "flarecheck-warn" $ text "Empty"
               | otherwise = do
      H.table $
        H.tr $ foldMap (H.td <<< H.pre <<< text <<< show) val

    null = foldr (\_ _ -> false) true

-- | A `createUI` implementation for types with a `Generic` instance.
gCreateUI :: forall a e. (Generic a) => UI e a -> UI e Renderable
gCreateUI ui = createUI (toSpine <$> ui)

-- | Takes a CSS classname and a `String` and returns a 'syntax highlighted'
-- | version of the `String`.
highlight :: String -> String -> H.Markup
highlight syntaxClass value =
  H.span ! HA.className ("flarecheck-" <> syntaxClass) $ text value

instance interactiveNumber :: Interactive Number where
  createUI = map (SetHTML <<< H.pre <<< highlight "number" <<< show)

instance interactiveInt :: Interactive Int where
  createUI = map (SetHTML <<< H.pre <<< highlight "number" <<< show)

instance interactiveString :: Interactive String where
  createUI = map (SetHTML <<< pretty)
    where
      pretty val = do H.pre $ highlight "string" (show val)
                      text ("String length: " <> show (length val))

instance interactiveChar :: Interactive Char where
  createUI = map (SetHTML <<< H.pre <<< highlight "string" <<< show)

instance interactiveBoolean :: Interactive Boolean where
  createUI = map (SetHTML <<< pretty)
    where
      pretty true =  H.pre ! HA.className "flarecheck-okay" $
                       highlight "boolean" "true"
      pretty false = H.pre ! HA.className "flarecheck-warn" $
                       highlight "boolean" "false"

instance interactiveOrdering :: Interactive Ordering where
  createUI = showCreateUI

instance genericSpineInteractive :: Interactive GenericSpine where
  createUI = map (SetHTML <<< H.pre <<< pretty)
    where
      pretty :: GenericSpine -> H.Markup
      pretty (SProd s arr) =
        if A.null arr
        then text s
        else do
          text s
          for_ arr \f -> do
            text " "
            pretty (f unit)
      pretty (SRecord arr) = do
        text "{"
        intercalate (text ", ") (map recEntry arr)
        text "}"
          where
            recEntry x = do
              text x.recLabel
              text ": "
              pretty (x.recValue unit)
      pretty (SBoolean x)  = highlight "boolean" (show x)
      pretty (SInt x)      = highlight "number" (show x)
      pretty (SNumber x)   = highlight "number" (show x)
      pretty (SString x)   = highlight "string" (show x)
      pretty (SChar x)     = highlight "string" (show x)
      pretty (SArray arr)  = do
        text "["
        intercalate (text ", ") (map (\x -> pretty (x unit)) arr)
        text "]"

instance interactiveMaybe :: (Show a) => Interactive (Maybe a) where
  createUI = map (SetHTML <<< pretty)
    where
      pretty Nothing  = H.pre ! HA.className "flarecheck-warn" $
                          H.b (text "Nothing")
      pretty (Just v) = H.pre ! HA.className "flarecheck-okay" $ do
                          H.b (text "Just")
                          text (" (" <> show v <> ")")

instance interactiveEither :: (Show a, Show b) => Interactive (Either a b) where
  createUI = map (SetHTML <<< pretty)
    where
      pretty (Left v)  = H.pre ! HA.className "flarecheck-warn" $ do
                           H.b (text "Left")
                           text (" (" <> show v <> ")")
      pretty (Right v) = H.pre ! HA.className "flarecheck-okay" $ do
                           H.b (text "Right")
                           text (" (" <> show v <> ")")

instance interactiveTuple :: (Show a, Show b) => Interactive (Tuple a b) where
  createUI = showCreateUI

instance interactiveArray :: (Show a) => Interactive (Array a) where
  createUI = map (SetHTML <<< pretty)
    where
      pretty [] = H.table $ H.tr $ H.td $
                    H.pre ! HA.className "flarecheck-warn" $ text "Empty Array"
      pretty val = do
        H.table $
          H.tr $ foldMap (H.td <<< H.pre <<< text <<< show) val

instance interactiveList :: (Show a) => Interactive (List a) where
  createUI = foldableCreateUI

instance interactiveFunction :: (Flammable a, Interactive b) => Interactive (a -> b) where
  createUI f = createUI (f <*> spark)

-- | Append a new interactive test. The arguments are the ID of the parent
-- | element, the title for the test, a documentation string and the list of
-- | Flare components. Returns the element for the output of the test.
foreign import appendTest :: forall e. ElementId
                          -> String
                          -> String
                          -> Array Element
                          -> Eff (dom :: DOM | e) Element

-- | Escape HTML special characters
foreign import escapeHTML :: String -> String

-- | Same as `text` from Smolder, but escapes special characters
text :: String -> H.Markup
text s = H.text (escapeHTML s)

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
-- | The String argument is an optional documentation string.
flareDoc' :: forall t e. (Interactive t)
            => ElementId
            -> Label
            -> Maybe String
            -> t
            -> Eff (chan :: Chan, dom :: DOM | e) Unit
flareDoc' parentId title doc x = do
  let flare = createUI (pure x)
  { components, signal } <- setupFlare flare
  let docString = fromMaybe "" doc
  output <- appendTest parentId title docString components
  runSignal (render output <$> signal)

-- | Run an interactive test. The label provides a title for the test. The
-- | String argument is an optional documentation string.
flareDoc :: forall t e. (Interactive t)
            => Label
            -> Maybe String
            -> t
            -> Eff (chan :: Chan, dom :: DOM | e) Unit
flareDoc = flareDoc' "tests"

-- | Run an interactive test. The ID specifies the parent element to which
-- | the test will be appended and the label provides a title for the test.
flareCheck' :: forall t e. (Interactive t)
            => ElementId
            -> Label
            -> t
            -> Eff (chan :: Chan, dom :: DOM | e) Unit
flareCheck' id label = flareDoc' id label Nothing

-- | Run an interactive test. The label provides a title for the test.
flareCheck :: forall t e. (Interactive t)
            => Label
            -> t
            -> Eff (chan :: Chan, dom :: DOM | e) Unit
flareCheck = flareCheck' "tests"
