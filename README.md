![Sparkle](html/sparkle.svg)

A PureScript library to create reactive web interfaces from type signatures.

- **[Live demo and tutorial](http://sharkdp.github.io/purescript-sparkle/)**
- [Try Flare](http://try.purescript.org/?backend=flare) - Write and compile Sparkle UIs in your browser
- [Talk](https://www.youtube.com/watch?v=iTSosG7vUyI) - A talk I gave about Flare and Sparkle (called FlareCheck back then) at LambdaConf 2016
- [Module documentation](http://pursuit.purescript.org/packages/purescript-sparkle/)
- [Flare: underlying UI library](https://github.com/sharkdp/purescript-flare)

## Usage

Suppose you have written a function
``` purs
substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)
```
that you want to test. Simply call (the first argument a title for the test):
``` purs
sparkle "substring" substring
```
This automatically creates an interactive testing environment (see [demo page](http://sharkdp.github.io/purescript-sparkle/) for a working version) like this:

[![](http://i.imgur.com/AxnoA5j.png)](http://sharkdp.github.io/purescript-sparkle/)

Notice how the type information is used to create appropriate input fields for `Int` and `String`. Similarly, calling
```purs
sparkle "filter even" (filter even)
```
creates the following interface:

[![](http://i.imgur.com/KDg8KfD.png)](http://sharkdp.github.io/purescript-sparkle/)

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
import Sparkle

main = sparkle "substring" substring
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
See the [assets](assets) folder for full HTML and CSS templates.
