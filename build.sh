#!/usr/bin/bash

ghc -c -O src/haskell/Stack.hs -outputdir tmp -stubdir include/stubs -Iinclude -itmp
ghc -c -O src/haskell/PDA.hs -outputdir tmp -stubdir include/stubs -Iinclude -itmp
ghc -c -O src/haskell/TypeDeclarations.hs -outputdir tmp -stubdir include/stubs -Iinclude -itmp
ghc -c -O src/haskell/CGets.hs -outputdir tmp -stubdir include/stubs -Iinclude -itmp
ghc -c -O src/haskell/Assembly.hs -outputdir tmp -stubdir include/stubs -Iinclude -itmp
ghc -c -O src/haskell/Helpers.hs -outputdir tmp -stubdir include/stubs -Iinclude -itmp
ghc -c -O src/haskell/ErrorCheck.hs -outputdir tmp -stubdir include/stubs -Iinclude -itmp
ghc -c -O src/haskell/Parser.hs -outputdir tmp -stubdir include/stubs -Iinclude -itmp

ghc --make src/c/* src/haskell/* -no-hs-main -stubdir include/stubs -outputdir tmp -Llib -Iinclude -o bin/syntan