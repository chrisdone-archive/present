present
=======

Make presentations for data types.

## Install

Requires: GHC 7.10.3

Install globally into your GHC's global database (via stack) with:

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
:def presentDebug \e -> return ("let it = " ++ e ++ "\n$(Present.presentIt)")
:def presentQualified \e -> return ("let it = " ++ e ++ "\nPrelude.putStrLn (Present.toShow True $(Present.presentIt))")
:def present \e -> return ("let it = " ++ e ++ "\nPrelude.putStrLn (Present.toShow False $(Present.presentIt))")
:set -package present
```

## Usage

``` haskell
bash-3.2$ stack exec ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
package flags have changed, resetting and loading new packages...
λ> :present "hi"
"hi"

λ> :present 123
123

λ> data X a = X a Int

λ> :present X 5 1
X 5 1

λ> :present S88.pack "hi"
PS (ForeignPtr GHC.Prim.Addr# (PlainPtr GHC.Prim.MutableByteArray#)) 0 2

λ> :present print
<a_0 -> GHC.Types.IO ()>

λ> :present print ()
IO (<GHC.Prim.State# GHC.Prim.RealWorld -> (GHC.Prim.State# GHC.Prim.RealWorld, a_0)>)

λ> :present undefined
_ :: t_0

λ> :present id
<a_0 -> a_0>

λ> :present [undefined]
[_ :: t_0]

```
