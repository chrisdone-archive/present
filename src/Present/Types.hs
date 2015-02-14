{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Presentation types.

module Present.Types
  (Mode(..)
  ,Present(..)
  ,Presentation(..)
  ,Type(..)
  ,Field(..)
  ,Constructor(..)
  ,Cursor(..))
  where

import Data.Monoid
import Data.Proxy
import Data.String

-- | The presentation mode.
data Mode
  = Internal  -- ^ Show internal implementation of the data type e.g. @1 : 2 : []@.
  | External  -- ^ Show an opaque external representation e.g. @[1,2,3]@.
  deriving (Eq,Show)

-- | Things which can be presented in a uniform manner.
class Present a where
  presentValue :: Mode
               -> Cursor       -- ^ History of cursors.
               -> Cursor       -- ^ Cursor to stop at.
               -> a            -- ^ Value to present at the given cursor.
               -> Presentation -- ^ A presentation for the value at the given cursor.
  presentType :: Proxy a -> Type
  -- ^ Useful for container types which want to show the type of its
  -- child types without needing a value. E.g. a list may have zero
  -- elements, but with 'presentType' you have a convenient way to get
  -- the type.

-- | A type's display.
newtype Type =
  Type {typeString :: String}
  deriving (IsString,Monoid,Show)

-- | A field name.
newtype Field = Field { fieldString :: String }
  deriving (Show)

-- | A constructor name.
newtype Constructor = Constructor { constructorName :: String }
  deriving (Show,IsString)

-- | A cursor into a data structure.
newtype Cursor = Cursor { cursorInts :: [Integer] }
  deriving (Monoid,Show)

-- | A presentation of a level of a data type.
data Presentation
  = Integral !Type !Constructor
  -- ^ An integral presentation (Int, Integer, etc.).
  | Floating !Type !Constructor
  -- ^ A floating point (Float, Double, etc.)
  | Char !Type !String
  -- ^ A character presentation.
  | String !Type !(Maybe ((Type,Cursor),(Type,Cursor)))
  -- ^ A string presentation. Either empty or a head and a tail.
  | Tuple !Type ![(Type,Cursor)]
  -- ^ A tuple presentation of many differing types.
  | List !Type !(Maybe ((Type,Cursor),(Type,Cursor)))
  -- ^ A list presentation. Either empty or a head and a tail.
  | Alg !Type !Constructor ![(Type,Cursor)]
  -- ^ An algebraic data type with many types inside.
  | Record !Type !Constructor ![(Field,(Type,Cursor))]
  -- ^ A record data type with many named types inside.
  deriving (Show)
