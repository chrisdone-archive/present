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

λ> :present (5.2,"hi",1,'a')
(5.2,"hi",1,'a')

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

## Extension

You can write your own instances like this:

``` haskell
λ> data X = X Int
λ> :present X 5
X 5
λ> instance Present0 X where present0 = ("X",\(X x) -> IntegerPresentation "X" (show x))
λ> :present X 5
5
```

## Detailed output

``` haskell
λ> :presentDebug (2.5,"hi",1,'a')
TuplePresentation
  "(GHC.Types.Double,GHC.Base.String,GHC.Integer.Type.Integer,GHC.Types.Char)"
  [ChoicePresentation
     "GHC.Types.Double"
     [("Floating",IntegerPresentation "GHC.Types.Double" "2.5")
     ,("Show",IntegerPresentation "GHC.Types.Double" "2.5")
     ,("Rational",IntegerPresentation "GHC.Types.Double" "5/2")
     ,("Internal"
      ,DataTypePresentation "GHC.Types.Double"
                            "GHC.Types.D#"
                            [PrimitivePresentation "GHC.Prim.Double#"])]
  ,ChoicePresentation
     "String"
     [("String",StringPresentation "String" "hi")
     ,("List of characters"
      ,ListPresentation
         "[GHC.Types.Char]"
         [ChoicePresentation
            "GHC.Types.Char"
            [("Character",CharPresentation "GHC.Types.Char" "h")
            ,("Unicode point"
             ,ChoicePresentation
                "GHC.Types.Char"
                [("Decimal",IntegerPresentation "GHC.Types.Char" "104")
                ,("Hexadecimal",IntegerPresentation "GHC.Types.Char" "68")
                ,("Binary",IntegerPresentation "GHC.Types.Char" "1101000")])
            ,("Internal"
             ,DataTypePresentation "GHC.Types.Char"
                                   "GHC.Types.C#"
                                   [PrimitivePresentation "GHC.Prim.Char#"])]
         ,ChoicePresentation
            "GHC.Types.Char"
            [("Character",CharPresentation "GHC.Types.Char" "i")
            ,("Unicode point"
             ,ChoicePresentation
                "GHC.Types.Char"
                [("Decimal",IntegerPresentation "GHC.Types.Char" "105")
                ,("Hexadecimal",IntegerPresentation "GHC.Types.Char" "69")
                ,("Binary",IntegerPresentation "GHC.Types.Char" "1101001")])
            ,("Internal"
             ,DataTypePresentation "GHC.Types.Char"
                                   "GHC.Types.C#"
                                   [PrimitivePresentation "GHC.Prim.Char#"])]])]
  ,ChoicePresentation
     "GHC.Integer.Type.Integer"
     [("Decimal",IntegerPresentation "GHC.Integer.Type.Integer" "1")
     ,("Hexadecimal",IntegerPresentation "GHC.Integer.Type.Integer" "1")
     ,("Binary",IntegerPresentation "GHC.Integer.Type.Integer" "1")
     ,("Internal"
      ,DataTypePresentation "GHC.Integer.Type.Integer"
                            "GHC.Integer.Type.S#"
                            [PrimitivePresentation "GHC.Prim.Int#"])]
  ,ChoicePresentation
     "GHC.Types.Char"
     [("Character",CharPresentation "GHC.Types.Char" "a")
     ,("Unicode point"
      ,ChoicePresentation
         "GHC.Types.Char"
         [("Decimal",IntegerPresentation "GHC.Types.Char" "97")
         ,("Hexadecimal",IntegerPresentation "GHC.Types.Char" "61")
         ,("Binary",IntegerPresentation "GHC.Types.Char" "1100001")])
     ,("Internal"
      ,DataTypePresentation "GHC.Types.Char"
                            "GHC.Types.C#"
                            [PrimitivePresentation "GHC.Prim.Char#"])]]
```
