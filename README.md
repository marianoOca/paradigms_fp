# 🚀 Spacecraft Simulation - Haskell Functional Programming

This project was developed as part of the **Programming Language Paradigms** course (1st semester 2023, TP1). It models spacecrafts built from modular components using recursive data types and functional paradigms in Haskell; following the Haskell2010 standard.

## 📄 Project Overview

Spacecraft are modeled using recursive data structures, allowing them to be composed from basic components: containers, engines, shields, and cannons. The simulation also includes how these spacecrafts handle different types of space hazards.

## 🚀 Key Features

- `foldNave`: Custom structural recursion scheme over spacecraft data structure.
- `capacidad`: Calculates total storage capacity (containers).
- `poderDeAtaque`: Counts the number of cannons.
- `puedeVolar`: Checks if a spacecraft has at least one engine.
- `mismoPotencial`: Compares if two spacecrafts have the same set of components.
- `mayorCapacidad`: Returns the ship with the highest capacity in a non-empty list.
- `transformar`: Applies a transformation to each component in a spacecraft.
- `impactar`: Applies a space hazard to a spacecraft and updates its structure accordingly.
- `maniobrar`: Applies a sequence of hazards in order.
- `pruebaDeFuego`: Filters spacecrafts that survive a sequence of hazards.
- `componentesPorNivel`: Counts components at a specific level in the tree structure.
- `dimensiones`: Measures depth and maximum width of a spacecraft.

## ✅ Testing

Each function includes illustrative examples.

To install test libraries:
```bash
cabal install HUnit
```

## 📚 References

- [Haskell 2010 Language Report](http://www.haskell.org/onlinereport/haskell2010)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
- [Real World Haskell](http://book.realworldhaskell.org/read)
- [Hoogle](http://www.haskell.org/hoogle)
- [Hayoo!](http://holumbus.fh-wedel.de/hayoo/hayoo.html)
