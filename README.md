# FlareCheck

A library to create interactive test suites for PureScript functions.

- **[Live demo and tutorial](http://sharkdp.github.io/purescript-flarecheck/)**

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
The function `flareCheck` automatically creates an interactive testing environment (see [demo page](http://sharkdp.github.io/purescript-flarecheck/) for a working version):

![](http://i.imgur.com/EmwNL9X.png)

Notice how the type information is used to create appropriate input fields for `Int` and `String`. Similarly, calling
```purs
flareCheck "filter even" (filter even)
```
creates the following interface:

![](http://i.imgur.com/LXexxEm.png)
