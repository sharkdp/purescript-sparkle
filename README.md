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

## Examples

Suppose you have a function
``` purs
substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)
```
that takes two integers and a string. Sparkle can automatically create a user interface for these inputs by calling:
``` purs
sparkle "substring" substring
```
The result looks like this:

[![](http://i.imgur.com/AxnoA5j.png)](http://sharkdp.github.io/purescript-sparkle/)

Similarly, calling
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
