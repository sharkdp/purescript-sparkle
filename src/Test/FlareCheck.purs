module Test.FlareCheck
  ( class Flammable
  , spark
  , class Read
  , typeName
  , defaults
  , read
  , NonNegativeInt(..)
  , SmallInt(..)
  , SmallNumber(..)
  , Multiline(..)
  , WrapEnum(..)
  , class Interactive
  , interactive
  , interactiveGeneric
  , interactiveShow
  , interactiveFoldable
  , Renderable(..)
  , flareDoc'
  , flareDoc
  , flareCheck'
  , flareCheck
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Array as A
import Data.Array.Unsafe as AU
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Enum (class Enum, succ)
import Data.Foldable (class Foldable, for_, intercalate, foldl)
import Data.Generic (class Generic, GenericSpine(..), toSpine)
import Data.Int (fromString)
import Data.List (List(), toList)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (split, length, charAt, joinWith)
import Data.Tuple (Tuple(..))

import Global (readFloat, isFinite)

import Type.Proxy (Proxy(..))

import Signal.Channel (CHANNEL())

import DOM (DOM())
import DOM.Node.Types (Element())

import Text.Smolder.Markup (Markup, text) as H
import Text.Smolder.Markup ((!))
import Text.Smolder.HTML (pre, span) as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Renderer.String (render) as H

import Signal (runSignal)
import Flare (Label, ElementId, UI, setupFlare, fieldset, string, radioGroup,
             boolean, stringPattern, int, intRange, intSlider, number,
             numberSlider, select)

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
  spark = fieldset "Either" $
            toEither <$> radioGroup "Select:" "Left" ["Right"] id
                     <*> spark
                     <*> spark
    where toEither "Left" x _ = Left x
          toEither _      _ y = Right y

-- | A newtype for non-negative integer values.
newtype NonNegativeInt = NonNegativeInt Int

instance flammableNonNegativeInt :: Flammable NonNegativeInt where
  spark = NonNegativeInt <$> intRange "Int" 0 top 1

-- | A newtype for small integer values in the range from 0 to 100.
newtype SmallInt = SmallInt Int

instance flammableSmallInt :: Flammable SmallInt where
  spark = SmallInt <$> intSlider "Int" 0 100 1

-- | A newtype for numbers in the closed interval from 0.0 and 1.0.
newtype SmallNumber = SmallNumber Number

instance flammableSmallNumber :: Flammable SmallNumber where
  spark = SmallNumber <$> numberSlider "Number" 0.0 1.0 0.00001 0.5

-- | A newtype for strings where "\n" is parsed as a newline
-- | (instead of "\\n").
newtype Multiline = Multiline String

instance flammableMultiline :: Flammable Multiline where
  spark = Multiline <$> toNewlines <$> string "String" "foo\\nbar"
    where
      toNewlines = split "\\n" >>> joinWith "\n"

newtype WrapEnum a = WrapEnum a

instance flammableWrapEnum :: (Enum a, Show a) => Flammable (WrapEnum a) where
  spark = WrapEnum <$> select "Enum" bottom (rest bottom) show
    where
      rest x = maybe [] (\y -> y `A.cons` (rest y)) (succ x)

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
-- | a Flare UI which returns a `Renderable` output.
class Interactive t where
  interactive :: forall e. UI e t -> UI e Renderable

-- | A default `interactive` implementation for any `Show`able type.
interactiveShow :: forall t e. (Show t) => UI e t -> UI e Renderable
interactiveShow = map (SetText <<< show)

-- | A default `interactive` implementation for `Foldable` types.
interactiveFoldable :: forall f a e. (Foldable f, Generic a)
                    => UI e (f a)
                    -> UI e Renderable
interactiveFoldable = map (SetHTML <<< H.pre <<< markup)
  where
    markup val = do
      text "fromFoldable "
      prettyPrint (fromFoldable val)

    -- TODO: fromFoldable should appear in Data.Array soon
    -- Inefficient version for the meantime:
    fromFoldable :: forall g b. Foldable g => g b -> Array b
    fromFoldable = foldl A.snoc []

-- | Takes a CSS classname and a `String` and returns a 'syntax highlighted'
-- | version of the `String`.
highlight :: String -> String -> H.Markup
highlight syntaxClass value =
  H.span ! HA.className ("flarecheck-" <> syntaxClass) $ text value

-- | Add a tooltip to an element.
tooltip :: String -> H.Markup -> H.Markup
tooltip tip = H.span ! HA.className "flarecheck-tooltip" ! HA.title tip

-- | Extract the constructor name from a string like `Data.Tuple.Tuple`.
constructor :: String -> H.Markup
constructor long = tooltip modString $ highlight "constructor" name
  where
    parts = split "." long
    name = AU.last parts
    modString =
      if A.length parts == 1
        then "Data constructor form unknown module"
        else long

-- | Pretty print a `GenericSpine`. This is an adapted version of
-- | `Data.Generic.genericShowPrec`.
prettyPrec :: Int -> GenericSpine -> H.Markup
prettyPrec d (SProd s arr) = do
  if (A.null arr)
    then constructor s
    else do
      showParen (d > 10) $ do
        constructor s
        for_ arr \f -> do
          text " "
          prettyPrec 11 (f unit)
  where showParen false x = x
        showParen true  x = do
          text "("
          x
          text ")"

prettyPrec d (SRecord arr) = do
  text "{ "
  intercalate (text ", ") (map recEntry arr)
  text " }"
    where
      recEntry x = do
        highlight "record-field" x.recLabel
        text ": "
        prettyPrec 0 (x.recValue unit)

prettyPrec d (SBoolean x)  = tooltip "Boolean" $ highlight "boolean" (show x)
prettyPrec d (SNumber x)   = tooltip "Number"  $ highlight "number" (show x)
prettyPrec d (SInt x)      = tooltip "Int"     $ highlight "number" (show x)
prettyPrec d (SString x)   = tooltip tip       $ highlight "string" (show x)
  where tip = "String of length " <> show (length x)
prettyPrec d (SChar x)     = tooltip tip       $ highlight "string" (show x)
  where tip = "Char (with char code " <> show (toCharCode x) <> ")"
prettyPrec d (SArray arr)  = tooltip tip $ do
  text "["
  intercalate (text ", ") (map (\x -> prettyPrec 0 (x unit)) arr)
  text "]"
    where tip = "Array of length " <> show (A.length arr)

-- | Pretty print a `GenericSpine`.
pretty :: GenericSpine -> H.Markup
pretty = prettyPrec 0

-- | Pretty print a value which has a `Generic` type.
prettyPrint :: forall a. Generic a => a -> H.Markup
prettyPrint = toSpine >>> pretty

-- | A default `interactive` implementation for types with a `Generic` instance.
interactiveGeneric :: forall a e. (Generic a) => UI e a -> UI e Renderable
interactiveGeneric ui = ((SetHTML <<< H.pre <<< prettyPrint) <$> ui)

instance interactiveNumber :: Interactive Number where
  interactive = interactiveGeneric

instance interactiveInt :: Interactive Int where
  interactive = interactiveGeneric

instance interactiveString :: Interactive String where
  interactive = interactiveGeneric

instance interactiveChar :: Interactive Char where
  interactive = interactiveGeneric

instance interactiveBoolean :: Interactive Boolean where
  interactive = map (SetHTML <<< markup)
    where
      classN true  = "flarecheck-okay"
      classN false = "flarecheck-warn"
      markup v = H.pre ! HA.className (classN v) $ prettyPrint v

instance interactiveOrdering :: Interactive Ordering where
  interactive = interactiveShow

instance genericSpineInteractive :: Interactive GenericSpine where
  interactive = map (SetHTML <<< H.pre <<< pretty)

instance interactiveMaybe :: Generic a => Interactive (Maybe a) where
  interactive = map (SetHTML <<< markup)
    where
      classN Nothing  = "flarecheck-warn"
      classN _        = ""
      markup v = H.pre ! HA.className (classN v) $ prettyPrint v

instance interactiveEither :: (Generic a, Generic b) => Interactive (Either a b) where
  interactive = map (SetHTML <<< markup)
    where
      classN (Left _) = "flarecheck-warn"
      classN _        = ""
      markup v = H.pre ! HA.className (classN v) $ prettyPrint v

instance interactiveTuple :: (Generic a, Generic b) => Interactive (Tuple a b) where
  interactive = interactiveGeneric

instance interactiveArray :: Generic a => Interactive (Array a) where
  interactive = map (SetHTML <<< markup)
    where
      classN [] = "flarecheck-warn"
      classN _  = ""
      markup v = H.pre ! HA.className (classN v) $ prettyPrint v

instance interactiveList :: Generic a => Interactive (List a) where
  interactive = interactiveFoldable

instance interactiveWrapEnum :: Generic a => Interactive (WrapEnum a) where
  interactive = interactiveGeneric <<< map unwrap
    where unwrap (WrapEnum e) = e

instance interactiveFunction :: (Flammable a, Interactive b) => Interactive (a -> b) where
  interactive f = interactive (f <*> spark)

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
            -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
flareDoc' parentId title doc x = do
  let flare = interactive (pure x)
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
            -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
flareDoc = flareDoc' "tests"

-- | Run an interactive test. The ID specifies the parent element to which
-- | the test will be appended and the label provides a title for the test.
flareCheck' :: forall t e. (Interactive t)
            => ElementId
            -> Label
            -> t
            -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
flareCheck' id label = flareDoc' id label Nothing

-- | Run an interactive test. The label provides a title for the test.
flareCheck :: forall t e. (Interactive t)
            => Label
            -> t
            -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
flareCheck = flareCheck' "tests"
