IT'S ALIVE
=========

I gave up on generating class instances from inside the REPL, instead
generating code on the fly with explicit dictionary passing:

``` haskell
λ> data X = X Char Int
data X = X Char Int
(0.00 secs, 510712 bytes)
λ> :present X 'a' 123

<interactive>:2650:5: Warning:
    This binding for ‘it’ shadows the existing binding
      defined at <interactive>:2648:1
it :: X
(0.00 secs, 517656 bytes)
<interactive>:2650:1-10: Splicing expression
    presentIt
  ======>
    let
      present_Ghci3_X :: X -> Presentation
      present_GHC_Types_Int :: Int -> Presentation
      present_GHC_Types_Char :: Char -> Presentation
      present_Ghci3_X
        = \case {
            X slot_1 slot_2
              -> Alg
                   "Ghci3.X"
                   [present_GHC_Types_Char slot_1, present_GHC_Types_Int slot_2] }
      present_GHC_Types_Int = present
      present_GHC_Types_Char = present
    in present_Ghci3_X it
Alg "Ghci3.X" [Integer "Char" "a",Integer "Int" "123"]
it :: Presentation
(0.01 secs, 3098272 bytes)
λ>
```

## From GHCi

``` haskell
:def present \e -> return ("let it = " ++ e ++ "\n$presentIt")
```
