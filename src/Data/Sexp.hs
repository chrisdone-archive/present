-- | A simple s-expression output library.

module Data.Sexp where

import Data.Char
import Data.List
import Data.String
import Text.Printf

-- | An s-expression.
data Sexp
  = Symbol String
  | String String
  | Integer Integer
  | Char Char
  | List [Sexp]

instance IsString Sexp where
  fromString = Symbol

-- | Prefix list.
plist :: [(String,Sexp)] -> Sexp
plist =
  List .
  concatMap (\(key,val) ->
               [Symbol (":" ++ key),val])

-- | Association list.
alist :: [(String,Sexp)] -> Sexp
alist =
  List .
  map (\(key,val) -> List [Symbol key,val])

-- | Print an s-expression to a string.
toString :: Sexp -> String
toString (Integer i) = show i
toString (Char i) = show (fromEnum i)
toString (Symbol s) = concatMap normalize s
  where normalize c
          | c == '-' || c == '.' || c == ':' =
            [c]
          | isAlphaNum c = [c]
          | otherwise =
            "\\" ++
            [c]
toString (String s) =
  "\"" ++
  concatMap normalize s ++
  "\""
  where normalize '"' = "\\\""
        normalize '\n' = "\\n"
        normalize c
          | isPrint c = [c]
          | otherwise =
            "\\u" ++
            printf "%04d" (fromEnum c)
toString (List s) =
  "(" ++
  intercalate " "
              (map toString s) ++
  ")"
