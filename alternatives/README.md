# Alternative implementations

This directory contains experiments in different
ways of encoding and solving the schematic plan problem.

1. Direct SAT grid encoding (Haskell program using [satplus](https://github.com/koengit/satplus/))
2. Linear programming encoding (Haskell program using the cbc solver (`cabal install limp-cbc`)) 
3. Level-based SAT encoding (Rust program)
4. Fronts-based SAT encoding (Haskell program using [satplus](https://github.com/koengit/satplus/))

![Table of results](perftable.png)

![Comparison figures](cmpfig.png)
