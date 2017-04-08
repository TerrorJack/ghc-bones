# ghc-bones

[![Build Status](https://travis-ci.org/TerrorJack/ghc-bones.svg)](https://travis-ci.org/TerrorJack/ghc-bones)

Yet another GHC API wrapper.

## Provides

* A `SessionT` monad transformer to encapsulate GHC API. It's `mtl`-compatible and implements a lot of useful instances.
* A test suite demostrating the use of `SessionT`:
    * Compile a module to object code
    * Load a compiled module and evaluate an expression

## Planned

* `Cabal` support
* `ghci` resource limiting
* More comprehensive test suite
