present
=====

Make presentations for data types.

### Breadth-first unpacking of data structures

``` haskell
data Foo = Foo Bar [Bar] Char Int
  deriving (Typeable,Data)

data Bar = Bar () Bool
  deriving (Typeable,Data)
```

The data type can be queried like this:

``` haskell
> let x = (Foo (Bar () True) [(Bar () True),(Bar () False)] 'a' 5)
> present (fromJust (fromList [0])) x
Just (Alg "Foo" ["@0→0","@0→1","@0→2","@0→3"])
> present (fromJust (fromList [0,1])) x
Just (Alg "(:)" ["@0→1→0","@0→1→1"])
> present (fromJust (fromList [0,1,0])) x
Just (Alg "Bar" ["@0→1→0→0","@0→1→0→1"])
> present (fromJust (fromList [0,1,0,0])) x
Just (Alg "()" [])
> present (fromJust (fromList [0,1,0,1])) x
Just (Alg "True" [])
> present (fromJust (fromList [0,2])) x
Just (Char "'a'")
> present (fromJust (fromList [0,3])) x
Just (Integer "5")
```

### Lazy infinite data structures

Data structures are also be unpacked lazily.

``` haskell
> present (fromJust (fromList [0])) [1..]
Just (Alg "(:)" ["@0→0","@0→1"])
> present (fromJust (fromList [0,0])) [1..]
Just (Integer "1")
> present (fromJust (fromList [0,1])) [1..]
Just (Alg "(:)" ["@0→1→0","@0→1→1"])
> present (fromJust (fromList [0,1,0])) [1..]
Just (Integer "2")
> present (fromJust (fromList [0,1,1,0])) [1..]
Just (Integer "3")
> present (fromJust (fromList [0,1,1,1,0])) [1..]
Just (Integer "4")
```
