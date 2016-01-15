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
```

A data type that describes possible output actions and values for an
interactive test.

#### `Interactive`

``` purescript
class Interactive t where
  createUI :: forall e. UI e t -> UI e Renderable
```

A type class for interactive tests. Instances must provide a way to create
a Flare UI which returns a `String` as output.

##### Instances
``` purescript
Interactive Number
Interactive Int
Interactive String
Interactive Char
Interactive Boolean
Interactive Ordering
Interactive GenericSpine
(Show a) => Interactive (Maybe a)
(Show a, Show b) => Interactive (Either a b)
(Show a, Show b) => Interactive (Tuple a b)
(Show a) => Interactive (Array a)
(Show a) => Interactive (List a)
(Flammable a, Interactive b) => Interactive (a -> b)
```

#### `showCreateUI`

``` purescript
showCreateUI :: forall t e. (Show t) => UI e t -> UI e Renderable
```

A default `createUI` implementation for any `Show`able type.

#### `foldableCreateUI`

``` purescript
foldableCreateUI :: forall f a e. (Foldable f, Show a) => UI e (f a) -> UI e Renderable
```

A default `createUI` implementation for `Foldable` types.

#### `gCreateUI`

``` purescript
gCreateUI :: forall a e. (Generic a) => UI e a -> UI e Renderable
```

A `createUI` implementation for types with a `Generic` instance.

#### `flareCheck'`

``` purescript
flareCheck' :: forall t e. (Interactive t) => ElementId -> Label -> t -> Eff (chan :: Chan, dom :: DOM | e) Unit
```

Run an interactive test. The ID specifies the parent element to which
the test will be appended and the label provides a title for the test.

#### `flareCheck`

``` purescript
flareCheck :: forall t e. (Interactive t) => Label -> t -> Eff (chan :: Chan, dom :: DOM | e) Unit
```

Run an interactive test. The label provides a title for the test.


