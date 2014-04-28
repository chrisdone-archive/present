present
=====

Make presentations for data types.

### Breadth-first unpacking of data structures

``` haskell
> import Data.Maybe
> import Data.Data
> import Data.Typeable
> import Data.ID
> :set -XDeriveDataTypeable
> data Bar = Bar () Bool deriving (Typeable,Data)
> data Foo = Foo Bar [Bar] Char Int deriving (Typeable,Data)
```

The data type can be queried like this:

``` haskell
> let x = Foo (Bar () True) [(Bar () True),(Bar () False)] 'a' 5
> present (fromJust (fromList [0])) x
Just (Alg "Foo" "Foo" [("Bar","@0→0"),("[Bar]","@0→1"),("Char","@0→2"),("Int","@0→3")])
> present (fromJust (fromList [0,1])) x
Just (List "[Bar]" [("Bar","@0→1→0"),("[Bar]","@0→1→1")])
> present (fromJust (fromList [0,1,0])) x
Just (Alg "Bar" "Bar" [("()","@0→1→0→0"),("Bool","@0→1→0→1")])
> present (fromJust (fromList [0,1,0,0])) x
Just (Tuple "()" [])
> present (fromJust (fromList [0,1,0,1])) x
Just (Alg "Bool" "True" [])
> present (fromJust (fromList [0,2])) x
Just (Char "Char" "'a'")
> present (fromJust (fromList [0,3])) x
Just (Integer "Int" "5")
```

### Lazy infinite data structures

Data structures can also be unpacked lazily.

``` haskell
> present (fromJust (fromList [0])) [1..]
Just (List "[Integer]" [("Integer","@0→0"),("[Integer]","@0→1")])
> present (fromJust (fromList [0,0])) [1..]
Just (Integer "Integer" "1")
> present (fromJust (fromList [0,1])) [1..]
Just (List "[Integer]" [("Integer","@0→1→0"),("[Integer]","@0→1→1")])
> present (fromJust (fromList [0,1,0])) [1..]
Just (Integer "Integer" "2")
> present (fromJust (fromList [0,1,1,0])) [1..]
Just (Integer "Integer" "3")
> present (fromJust (fromList [0,1,1,1,0])) [1..]
Just (Integer "Integer" "4")
```

## Lazy memoization

Sharing allows you to inspect a data structure and only evaluate parts
of it once, subsequent calls will be immediate.

``` haskell
> let ack 0 n = n+1; ack m 0 = ack (m-1) 1; ack m n = ack (m-1) (ack m (n-1))
> :set +s
> let x = ack 3 8; xs = [x,x]
(0.00 secs, 1028952 bytes)
> present (fromJust (fromList [0])) xs
Just (List "[Integer]" [("Integer","@0→0"),("[Integer]","@0→1")])
(0.00 secs, 1035472 bytes)
> present (fromJust (fromList [0,0])) xs
Just (Integer "Integer" "2045")
(2.20 secs, 803045032 bytes)
> present (fromJust (fromList [0,1,0])) xs
Just (Integer "Integer" "2045")
(0.00 secs, 1070976 bytes)
```

## Representations for common editor-compatible formats

Support for JSON:

``` haskell
> fmap Data.Aeson.encode (present (fromJust (fromList [0])) (Foo (Bar () True) [] 'a' 6))
Just (Chunk "{\"slots\":[[\"Bar\",[0,0]],[\"[Bar]\",[0,1]],
[\"Char\",[0,2]],[\"Int\",[0,3]]],\"text\":\"Foo\",
\"rep\":\"alg\",\"type\":\"Foo\"}" Empty)
```

And for s-expressions (Emacs):

``` haskell
> fmap Data.AttoLisp.encode (present (fromJust (fromList [0])) (Foo (Bar () True) [] 'a' 6))
Just (Chunk "((rep \"alg\") (type \"Foo\") (text \"Foo\")
(slots ((\"Bar\" (0 0)) (\"[Bar]\" (0 1))
(\"Char\" (0 2)) (\"Int\" (0 3)))))" Empty)
```
