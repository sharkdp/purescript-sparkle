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
Flammable Boolean
(Flammable a, Flammable b) => Flammable (Tuple a b)
(Flammable a) => Flammable (Maybe a)
(Flammable a, Flammable b) => Flammable (Either a b)
```

#### `Interactive`

``` purescript
class Interactive t where
  createUI :: forall e. UI e t -> UI e String
```

A type class for interactive tests. Instances must provide a way to create
a Flare UI which returns a `String` as output.

##### Instances
``` purescript
Interactive Number
Interactive Int
Interactive String
Interactive Boolean
(Show a) => Interactive (Maybe a)
(Show a, Show b) => Interactive (Either a b)
(Show a, Show b) => Interactive (Tuple a b)
(Flammable a, Interactive b) => Interactive (a -> b)
```

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


