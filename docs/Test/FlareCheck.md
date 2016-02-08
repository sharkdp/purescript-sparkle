## Module Test.FlareCheck

#### `Flammable`

``` purescript
class Flammable a where
  spark :: forall e. UI e a
```

A type class for input parameters for interactive tests. Instances for
type `a` must provide a way to create a Flare UI which holds a value of
type `a`.

##### Instances
``` purescript
Flammable Number
Flammable Int
Flammable String
Flammable Char
Flammable Boolean
(Flammable a, Flammable b) => Flammable (Tuple a b)
(Flammable a) => Flammable (Maybe a)
(Flammable a, Flammable b) => Flammable (Either a b)
(Read a) => Flammable (Array a)
(Read a) => Flammable (List a)
```

#### `Read`

``` purescript
class Read a where
  typeName :: Proxy a -> String
  defaults :: Proxy a -> String
  read :: String -> Maybe a
```

A class for types which can be parsed from a `String`. This class is used
to construct input fields for `Array a` and `List a`.

##### Instances
``` purescript
Read Number
Read Int
Read String
Read Char
Read Boolean
```

#### `Renderable`

``` purescript
data Renderable
  = SetText String
  | SetHTML Markup
```

A data type that describes possible output actions and values for an
interactive test.

#### `Interactive`

``` purescript
class Interactive t where
  interactive :: forall e. UI e t -> UI e Renderable
```

A type class for interactive tests. Instances must provide a way to create
a Flare UI which returns a `Renderable` output.

##### Instances
``` purescript
Interactive Number
Interactive Int
Interactive String
Interactive Char
Interactive Boolean
Interactive Ordering
Interactive GenericSpine
(Generic a) => Interactive (Maybe a)
(Generic a, Generic b) => Interactive (Either a b)
(Generic a, Generic b) => Interactive (Tuple a b)
(Generic a) => Interactive (Array a)
(Generic a) => Interactive (List a)
(Flammable a, Interactive b) => Interactive (a -> b)
```

#### `interactiveShow`

``` purescript
interactiveShow :: forall t e. (Show t) => UI e t -> UI e Renderable
```

A default `interactive` implementation for any `Show`able type.

#### `interactiveFoldable`

``` purescript
interactiveFoldable :: forall f a e. (Foldable f, Generic a) => UI e (f a) -> UI e Renderable
```

A default `interactive` implementation for `Foldable` types.

#### `interactiveGeneric`

``` purescript
interactiveGeneric :: forall a e. (Generic a) => UI e a -> UI e Renderable
```

A default `interactive` implementation for types with a `Generic` instance.

#### `flareDoc'`

``` purescript
flareDoc' :: forall t e. (Interactive t) => ElementId -> Label -> Maybe String -> t -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
```

Run an interactive test. The ID specifies the parent element to which
the test will be appended and the label provides a title for the test.
The String argument is an optional documentation string.

#### `flareDoc`

``` purescript
flareDoc :: forall t e. (Interactive t) => Label -> Maybe String -> t -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
```

Run an interactive test. The label provides a title for the test. The
String argument is an optional documentation string.

#### `flareCheck'`

``` purescript
flareCheck' :: forall t e. (Interactive t) => ElementId -> Label -> t -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
```

Run an interactive test. The ID specifies the parent element to which
the test will be appended and the label provides a title for the test.

#### `flareCheck`

``` purescript
flareCheck :: forall t e. (Interactive t) => Label -> t -> Eff (channel :: CHANNEL, dom :: DOM | e) Unit
```

Run an interactive test. The label provides a title for the test.


