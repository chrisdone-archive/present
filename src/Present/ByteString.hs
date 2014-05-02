{-# LANGUAGE DeriveDataTypeable #-}

-- | Presentable version of Data.ByteString.

module Present.ByteString where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Data

-- | Normalize a strict Data.ByteString to a simple type.
normalizeStrictByteString :: S.ByteString -> ByteString
normalizeStrictByteString = normalizeByteString . L.fromChunks . return

-- | Normalize Data.ByteString.Lazy to a simple, presentable data type.
normalizeByteString :: L.ByteString -> ByteString
normalizeByteString t =
  case L.uncons t of
    Just (x,xs) -> Cons (toEnum (fromEnum x)) (normalizeByteString xs)
    Nothing -> Nil

-- | A lazy, simple but clean presentable version of Data.ByteString.
data ByteString = Cons !Char ByteString | Nil
  deriving (Typeable,Data)
