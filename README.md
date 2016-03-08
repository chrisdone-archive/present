IT'S ALIVE
=========

I gave up on generating class instances from inside the REPL, instead
generating code on the fly with explicit dictionary passing:

``` haskell
bash-3.2$ stack build
present-4.0.0: unregistering (local file changes: src/Present.hs)
present-4.0.0: configure
Configuring present-4.0.0...
present-4.0.0: build
Preprocessing library present-4.0.0...
[1 of 2] Compiling Control.Monad.Trans.State.Strict ( src/Control/Monad/Trans/State/Strict.hs, .stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/Control/Monad/Trans/State/Strict.o )
[2 of 2] Compiling Present          ( src/Present.hs, .stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/Present.o )
In-place registering present-4.0.0...
present-4.0.0: copy/register
Installing library in
/Users/chris/Projects/present/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/lib/x86_64-osx-ghc-7.10.3/present-4.0.0-9q8cvmRl9VV3YGAw4tZ7Ch
Registering present-4.0.0...
```

```
bash-3.2$ stack exec ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
unknown option: 'c'
package flags have changed, resetting and loading new packages...
λ> :present 1
1
λ> :present (123,"abc",'a',Just 666)
(123,"abc",'a',GHC.Base.Just 666)
λ> :present [(123,"abc",'a',Just 666)]
[(123,"abc",'a',GHC.Base.Just 666)]
λ> data X a = X { foo :: Either a Int, bar :: Char}
λ> :present [X {foo = Left 123,bar = 'b'}]
[Ghci26.X {Ghci26.foo = (Data.Either.Left 123),Ghci26.bar = 'b'}]
λ> :present [X {foo = Left 123,bar = 'b'}]
[Ghci26.X {Ghci26.foo = (Data.Either.Left 123),Ghci26.bar = 'b'}]
λ> :presentDebug [(123,"abc",'a',Just 666)]
List "[(GHC.Integer.Type.Integer,[Prelude.Char],Prelude.Char,(GHC.Base.Maybe GHC.Integer.Type.Integer))]"
     [Tuple "(GHC.Integer.Type.Integer,[Prelude.Char],Prelude.Char,(GHC.Base.Maybe GHC.Integer.Type.Integer))"
            [Integer "GHC.Integer.Type.Integer" "123"
            ,String "String" "abc"
            ,Char "Prelude.Char" "a"
            ,Algebraic "(GHC.Base.Maybe GHC.Integer.Type.Integer)"
                       "GHC.Base.Just"
                       [Integer "GHC.Integer.Type.Integer" "666"]]]
λ>
```

## From GHCi

Put this in your .ghci

``` haskell
:seti -XTemplateHaskell
:def presentDebug \e -> return ("let it = " ++ e ++ "\n$(Present.presentIt)")
:def present \e -> return ("let it = " ++ e ++ "\nputStrLn (Present.toShow $(Present.presentIt))")
:set -package present
```
