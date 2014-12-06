{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Presentation types.

module Present.Types
  (Present(..)
  ,Presentation(..)
  ,Type(..)
  ,Field(..)
  ,Constructor(..)
  ,Cursor(..))
  where

import Data.Monoid
import Data.Proxy
import Data.String
import Data.Text (Text)

-- | Things which can be presented in a uniform manner.
class Present a where
  presentValue :: Cursor -> Cursor -> a -> Presentation
  presentType :: Proxy a -> Type

-- | A type's display.
newtype Type =
  Type {typeText :: Text}
  deriving (IsString,Monoid,Show)

-- | A field name.
newtype Field = Field { fieldText :: Text }
  deriving (Show)

-- | A constructor name.
newtype Constructor = Constructor { constructorName :: Text }
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
  | Char !Type !Text
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
