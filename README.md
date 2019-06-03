# primitive-foreign

[![Hackage](https://img.shields.io/hackage/v/primitive-foreign.svg)](https://hackage.haskell.org/package/primitive-foreign)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.com/haskell-primitive/primitive-foreign.svg?branch=master)](https://travis-ci.com/haskell-primitive/primitive-foreign)
 
The goal of this library is to make it possible to avoid the duplicated code between `Storable` and `Prim` APIs when one is working mostly with the `primitive` or `contiguous` APIs, by using the `Prim` interface to facilitate marshalling of values in memory.
