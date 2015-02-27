{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Presentation types.

module Present.Types
  (Mode(..)
  ,Present(..)
  ,Presentation(..)
  ,Type(..)
  ,Name
  ,Cursor(..)
  ,ConsFixity(..)
  ,Ty(..)
  ,Ident(..))
  where

import Data.Monoid
import Data.Proxy

import Language.Haskell.TH.Syntax

-- | The presentation mode.
data Mode
  = Internal  -- ^ Show internal implementation of the data type e.g. @1 : 2 : []@.
  | External  -- ^ Show an opaque external representation e.g. @[1,2,3]@.
  deriving (Eq,Show)

-- | Fixity of a constructor.
data ConsFixity
  = InfixCons
  | PrefixCons
  deriving (Eq,Show)

-- | Things which can be presented in a uniform manner.
class Present a where
  presentValue :: Mode
               -> Cursor       -- ^ History of cursors.
               -> Cursor       -- ^ Cursor to stop at.
               -> a            -- ^ Value to present at the given cursor.
               -> Presentation -- ^ A presentation for the value at the given cursor.
  presentType :: Proxy a -> Ty
  -- ^ Useful for container types which want to show the type of its
  -- child types without needing a value. E.g. a list may have zero
  -- elements, but with 'presentType' you have a convenient way to get
  -- the type. Typeable is no longer derivable, but this is better
  -- anyway because it allows more structure in the 'Ty' type.

-- | A cursor into a data structure.
newtype Cursor = Cursor { cursorInts :: [Int] }
  deriving (Monoid,Show)

-- | A presentation of a level of a data type.
data Presentation
  = Integral !Ty !Integer
  -- ^ An integral presentation (Int, Integer, etc.).
  | Ratio !Ty !Integer !Integer
  -- ^ A ratio.
  | Decimal !Ty !String
  -- ^ A floating point (Float, Double, etc.)
  | Char !Ty !Char
  -- ^ A character presentation.
  | String !Ty !(Maybe ((Ty,Cursor),(Ty,Cursor)))
  -- ^ A string presentation. Either empty or a head and a tail.
  | Tuple !Ty ![(Ty,Cursor)]
  -- ^ A tuple presentation of many differing types.
  | List !Ty !(Maybe ((Ty,Cursor),(Ty,Cursor)))
  -- ^ A list presentation. Either empty or a head and a tail.
  | Vector !Ty ![(Ty,Cursor)]
  -- ^ A vectorish type.
  | Alg !ConsFixity !Ty !Ident ![(Ty,Cursor)]
  -- ^ An algebraic data type with many types inside.
  | Record !Ty !Ident ![(Ident,(Ty,Cursor))]
  -- ^ A record data type with many named types inside.
  | Bottom !Ty !String
  deriving (Show)

-- | Ident.
data Ident =
  Ident !PkgName
        !ModName
        !OccName
  deriving (Show)

instance Lift Ident where
  lift (Ident (PkgName pkg) (ModName md) (OccName occ)) =
    [|Ident (PkgName pkg) (ModName md) (OccName occ)|]

-- | Type.
data Ty
  = TyCon !Ident
  | TyTuple ![Ty]
  | TyList !Ty
  | TyApp !Ty
          !Ty
  deriving (Show)
