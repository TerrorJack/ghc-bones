# ghc-bones

[![Build Status](https://travis-ci.org/TerrorJack/ghc-bones.svg)](https://travis-ci.org/TerrorJack/ghc-bones)

Yet another GHC API wrapper.

## Provides

* A `SessionT` monad transformer to encapsulate GHC API. It's `mtl`-compatible and implements a lot of useful instances.
* An `eval` function which compiles a module and uses it as the context to evaluate an expression. It supports:
    * Limiting time/memory usage
    * Evaluating the expression to normal form
* A `dumpCore` function which compiles some modules and dump the optimized Core. The dumper is not invoked if the cached interface/object files are up to date.
* A test suite demostrating the use of `SessionT`:
    * Compiling two modules to object code
    * Loading a compiled module and evaluate an expression
    * Compile modules and use a dummy counter as Core dumper

## Planned

* `Cabal` support
* Improve safety for `eval`, like scanning for unsafe identifiers by parsing & traversing the AST
* More comprehensive test suite
