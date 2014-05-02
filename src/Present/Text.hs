{-# LANGUAGE DeriveDataTypeable #-}

-- | Presentable version of Data.Text.

module Present.Text where

import           Data.Data
import qualified Data.Text as S
import qualified Data.Text.Lazy as L

-- | Normalize a strict Data.Text to a simple type.
normalizeStrictText :: S.Text -> Text
normalizeStrictText = normalizeText . L.fromStrict

-- | Normalize Data.Text.Lazy to a simple, presentable data type.
normalizeText :: L.Text -> Text
normalizeText t =
  case L.uncons t of
    Just (x,xs) -> Cons x (normalizeText xs)
    Nothing -> Nil

-- | A lazy, simple but clean presentable version of Data.Text.
data Text = Cons !Char Text | Nil
  deriving (Typeable,Data)
