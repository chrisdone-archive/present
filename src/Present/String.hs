{-# LANGUAGE DeriveDataTypeable #-}

-- | Presentable version of String.

module Present.String where

import           Data.Data
import qualified Prelude as P
import           Prelude hiding (String)

-- | Normalize String to a simple, presentable data type.
normalizeString :: P.String -> String
normalizeString (x:xs) = Cons x (normalizeString xs)
normalizeString [] = Nil

-- | A lazy, simple but clean presentable version of Data.String.
data String = Cons !Char String | Nil
  deriving (Typeable,Data)
