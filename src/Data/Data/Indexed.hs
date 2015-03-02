{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Indexed maps over data.

module Data.Data.Indexed
  (gmapI
  ,gindex
  ,gappend)
  where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Data
import           Data.Data.Exists
import           Data.Maybe

-- | Map over a data type tracking some number index.
gmapI :: (Data a, Num i,MonadState i m)
      => (forall d. Data d => d -> i -> m ())
      -> a
      -> m a
gmapI f y = gmapM go y
  where
    go d =
      do i <- get
         let !i' = i + 1
         put i'
         f d i
         return d

-- | Lookup a value at a (zero-based) index in the given data
-- structure.
gindex :: (Eq i, Num i,Data a)
       => i
       -> a
       -> Maybe D
gindex j x =
  listToMaybe (evalState (execWriterT (gmapI grab x)) 0)
  where grab d i =
          when (i == j)
               (tell [D d])

-- | Generically append over the indexed values of a data structure.
gappend :: (Num i,Data a,Monoid m)
        => (forall d. Data d => d -> i -> m)
        -> a
        -> m
gappend f x =
  evalState (execWriterT (gmapI (\d i -> tell (f d i)) x)) 0
