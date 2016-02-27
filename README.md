IT'S ALIVE
=========

I gave up on generating class instances from inside the REPL, instead
generating code on the fly with explicit dictionary passing:

``` haskell
λ> Right 5 :: Either Char Int
Right 5
it :: Either Char Int
(0.00 secs, 1037696 bytes)
λ> $presentIt
<interactive>:2100:1-10: Splicing expression
    presentIt
  ======>
    let
      present_GHC_Types_Int
        = \case {
            ghc-prim-0.4.0.0:GHC.Types.I# slot_1
              -> Alg "GHC.Types.I#" [(\ _ -> Primitive "GHC.Prim.Int#") slot_1] }
      present_GHC_Types_Char
        = \case {
            ghc-prim-0.4.0.0:GHC.Types.C# slot_1
              -> Alg
                   "GHC.Types.C#" [(\ _ -> Primitive "GHC.Prim.Char#") slot_1] }
      present_Data_Either_Either
        = (\ present_a_1627409552
             -> (\ present_b_1627409553
                   -> \case {
                        Left slot_1 -> Alg "Data.Either.Left" [present_a_1627409552 slot_1]
                        Right slot_1
                          -> Alg "Data.Either.Right" [present_b_1627409553 slot_1] }))
    in
      present_Data_Either_Either
        present_GHC_Types_Char present_GHC_Types_Int it
Alg "Data.Either.Right" [Alg "GHC.Types.I#" [Primitive "GHC.Prim.Int#"]]
```

## From GHCi

``` haskell
:def present \e -> return ("let it = " ++ e ++ "\n$presentIt")
```
