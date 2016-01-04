# FlareCheck

A library to create interactive test suites for PureScript functions.

- **[Live demo and tutorial](http://sharkdp.github.io/purescript-flarecheck/)**
- [Module documentation](http://pursuit.purescript.org/packages/purescript-flarecheck/)
- [Flare: underlying UI library](https://github.com/sharkdp/purescript-flare)

## Usage

Suppose you have written a function
``` purs
substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)
```
that you want to test. To use FlareCheck, you simply call (the first argument is just a title for the test):
``` purs
flareCheck "substring" substring
```
This automatically creates an interactive testing environment (see [demo page](http://sharkdp.github.io/purescript-flarecheck/) for a working version) like this:

[![](http://i.imgur.com/EmwNL9X.png)](http://sharkdp.github.io/purescript-flarecheck/)

Notice how the type information is used to create appropriate input fields for `Int` and `String`. Similarly, calling
```purs
flareCheck "filter even" (filter even)
```
creates the following interface:

[![](http://i.imgur.com/LXexxEm.png)](http://sharkdp.github.io/purescript-flarecheck/)

## Minimal setup

- The module that you want to test (`src/MyModule.purs`):
``` purs
module MyModule (substring) where

import Prelude
import Data.String

substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)
```
- A module which runs the tests (`test/Main.purs`):
``` purs
module Test.Main where

import Prelude
import MyModule
import Test.FlareCheck

main = flareCheck "substring" substring
```
- The compiled test module (`test.js`), run:
```
pulp build -O -I test -m Test.Main -t test.js
```
- An accompanying HTML file which includes an empty element with ID `tests` that runs the script:
``` HTML
<div id="tests"></div>
<script src="test.js"></script>
```
