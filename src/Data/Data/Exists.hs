{-# LANGUAGE ExistentialQuantification #-}

-- | An existential wrapper for data.

module Data.Data.Exists where

import Data.Data

-- | Data wrapper.
data D = forall a. Data a => D a
