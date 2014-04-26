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

## Lazy memoization

Sharing allows you to inspect a data structure and only evaluate parts
of it once, subsequent calls will be immediate.

``` haskell
> let ack 0 n = n+1; ack m 0 = ack (m-1) 1; ack m n = ack (m-1) (ack m (n-1))
> let xs = [ack 3 8,4]
> present (fromJust (fromList [0])) xs
Just (Alg "(:)" ["@0→0","@0→1"])
(0.00 secs, 1984104 bytes)
> present (fromJust (fromList [0,1])) xs
Just (Alg "(:)" ["@0→1→0","@0→1→1"])
(0.00 secs, 990528 bytes)
> present (fromJust (fromList [0,1,0])) xs
Just (Integer "4")
(0.00 secs, 1028944 bytes)
> present (fromJust (fromList [0,0])) xs
Just (Integer "2045")
(2.21 secs, 802929136 bytes)
> present (fromJust (fromList [0,0])) xs
Just (Integer "2045")
(0.00 secs, 1030392 bytes)
```

## Representations for common editor-compatible formats

Support for JSON:

``` haskell
> fmap Data.Aeson.encode (present (fromJust (fromList [0])) (Foo (Bar () True) [] 'a' 6))
Just (Chunk "{\"slots\":[[0,0],[0,1],[0,2],[0,3]],\"text\":\"Foo\",\"type\":\"alg\"}" Empty)
```

And for s-expressions (Emacs):

``` haskell
λ> fmap Data.AttoLisp.encode (present (fromJust (fromList [0])) (Foo (Bar () True) [] 'a' 6))
Just (Chunk "((type \"alg\") (text \"Foo\") (slots ((0 0) (0 1) (0 2) (0 3))))" Empty)
```
