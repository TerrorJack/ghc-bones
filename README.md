# ghc-bones

[![Build Status](https://travis-ci.org/TerrorJack/ghc-bones.svg)](https://travis-ci.org/TerrorJack/ghc-bones)

Yet another GHC API wrapper.

## Provides

* A `SessionT` monad transformer to encapsulate GHC API. It's `mtl`-compatible and implements a lot of useful instances.
* An `eval` function which compiles a module and use it as the context to evaluate an expression. It supports:
    * Limiting time/memory usage
    * Evaluating the expression to normal form
* A test suite demostrating the use of `SessionT`:
    * Compile a module to object code
    * Load a compiled module and evaluate an expression

## Planned

* `Cabal` support
* Improve safety for `eval`, like scanning for unsafe identifiers by parsing & traversing the AST
* More comprehensive test suite
