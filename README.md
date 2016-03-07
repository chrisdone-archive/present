IT'S ALIVE
=========

I gave up on generating class instances from inside the REPL, instead
generating code on the fly with explicit dictionary passing:

``` haskell
bash-3.2$ sh install.sh
Configuring present-4.0.0...
Building present-4.0.0...
Preprocessing library present-4.0.0...
[2 of 2] Compiling Present          ( src/Present.hs, dist/build/Present.o )
In-place registering present-4.0.0...
Installing library in
/usr/local/lib/x86_64-osx-ghc-7.10.3/present-4.0.0-9q8cvmRl9VV3YGAw4tZ7Ch
Registering present-4.0.0...
bash-3.2$ stack exec ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
unknown option: 'c'
package flags have changed, resetting and loading new packages...
Prelude> data X = X Int Char
Prelude> :present X 123 'a'
Alg "<TODO>" "Ghci1.X" [Integer "GHC.Types.Int" "123",Char "GHC.Types.Char" "a"]
λ>
```

## From GHCi

Put this in your .ghci

``` haskell
:seti -XTemplateHaskell
:def present \e -> return ("let it = " ++ e ++ "\n$(Present.presentIt)")
:set -package present
```
