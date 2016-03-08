present
=======

Make presentations for data types.

## Install

Install globally into your GHC's global database with:

```
$ sh install-to-global-db.sh
```

To remove it later:

```
$ sh remove-from-global-db.sh
```

To be added: install via `stack install present`, but it needs to be
added to an LTS/nightly version first.

## Customizing GHCi

Add to the following to your `~/.ghci`:

``` haskell
:seti -XTemplateHaskell
:def present \e -> return ("let it = " ++ e ++ "\nputStrLn (Present.toShow $(Present.presentIt))")
:set -package present
```

## Usage

``` haskell
bash-3.2$ stack exec ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
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
λ>
```
