<div align="center">
<h1>
<img src="html/sparkle.svg"
      alt="Sparkle"
      width="300">
</h1>
</div>

<p align="center">
<em>Automatically create reactive web interfaces from type signatures.</em>
</p>

<p align="center">
<b><a href="http://sharkdp.github.io/purescript-sparkle/">Live demo and tutorial</a></b>
| <a href="https://www.youtube.com/watch?v=iTSosG7vUyI">Conference talk</a>
| <a href="http://try.purescript.org/?backend=flare">Try in your browser</a>
| <a href="http://pursuit.purescript.org/packages/purescript-sparkle/">Documentation</a>
</p>

<hr>

Sparkle is a library that leverages the power of [PureScripts](http://purescript.org) type system to automatically create user interfaces based on type signatures.

The internal mechanism of this library is similar to [QuickCheck](https://github.com/purescript/purescript-quickcheck). However, instead of using randomly generated input data, Sparkle creates reactive web interfaces for "interactive testing". It uses the [Flare library](https://github.com/sharkdp/purescript-flare) to create those widgets.

## Example

Consider the following (hypothetic) function:
``` purs
formatNumber :: Number -> Int -> Char -> Boolean -> String
formatNumber value precision decimalMark isSigned = ...
```
Sparkle can automatically create a user interface for `formatNumber` by simply calling:
``` purs
sparkle "formatNumber" formatNumber
```
The result looks like this:

![Sparkle widget](https://i.imgur.com/xB13OGZ.png)

Notice how each input type (`Number`, `Int`, `Char`, `Boolean`) is represented by an appropriate input field.
Check out the **[demo page](http://sharkdp.github.io/purescript-sparkle/)** for an interactive version.

## Quick start

- Start a PureScript project in a new folder:
  ```
  pulp init
  ```

- Install *Sparkle*:
  ```
  bower install --save purescript-sparkle
  ```

- Write your own code (`src/MyModule.purs`), for example:

  ``` purs
  module MyModule (substring) where

  import Prelude
  import Data.String

  substring :: Int -> Int -> String -> String
  substring start end str = take (end - start) (drop start str)
  ```

- Write the module that creates the interactive Sparkle tests (`test/Main.purs`):

  ``` purs
  module Test.Main where

  import Prelude
  import MyModule
  import Sparkle

  main = sparkle "substring" substring
  ```

- Compile and bundle:
  ```
  pulp build -O -I test -m Test.Main -t test.js
  ```

- Copy the `index.html` and `sparkle.css` file from the `assets` folder:

  ``` bash
  cp bower_components/purescript-sparkle/assets/* .
  ```

- Open `index.html` in the browser.
