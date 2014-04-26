{-# LANGUAGE ExistentialQuantification #-}

-- | An existential wrapper for data.

module Data.Data.Exists
  (D(..))
  where

import Data.Data

-- | Data wrapper.
data D = forall a. Data a => D a
