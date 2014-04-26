{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | ID library

module Data.ID
  (ID
  ,split
  ,fromList
  ,singleton)
  where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup

newtype ID =
  ID (NonEmpty Integer)
  deriving (Show,Eq,Semigroup)

split :: ID -> (Integer,Maybe ID)
split (ID (x :| xs)) = (x,fromList xs)

fromList :: [Integer] -> Maybe ID
fromList = fmap ID . NE.nonEmpty

singleton :: Integer -> ID
singleton = ID . (:| [])
