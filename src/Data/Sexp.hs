-- | A simple s-expression output library.

module Data.Sexp where

import Data.Char
import Data.List
import Text.Printf

-- | An s-expression.
data Sexp
  = Symbol String
  | String String
  | List [Sexp]

-- | Print an s-expression to a string.
toString :: Sexp -> String
toString (Symbol s) = concatMap normalize s
  where normalize c
          | c == '-' || c == '.' = [c]
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
