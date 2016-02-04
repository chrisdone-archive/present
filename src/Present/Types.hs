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
  -- ^ An algebraic data type with many, unnamed types inside.
  | Record !Type !Constructor ![(Field,(Type,Cursor))]
  -- ^ A record-like mapping from name to field data type.
  | Choices !Type ![(PresentationType,Cursor)]
  -- ^ There is more than one choice for presenting this type.
  deriving (Show)

-- | A type of presentation. There are some standard ones and then
-- custom ones.
data PresentationType
  = Internal    -- ^ A presentation of the data structure's internals.
  | Opaque      -- ^ An opaque presentation of the data structure.
  | Other !String -- ^ A non-standard type of presentation.
  deriving (Show,Eq)

-- | Things which can be presented in a uniform manner.
class Present a  where
  presentValue
    :: Cursor -- ^ History, used as a breadcrumb to construct new search paths
    -> Cursor -- ^ Search path
    -> a      -- ^ Structure to dive into
    -> Presentation
  presentType
    :: Proxy a -- ^ Just a witness for the instance
    -> Type

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

instance Present Int where
  presentValue _ _ i =
    Integral (presentType (return i))
             (Constructor (show i))
  presentType _ = "Prelude.Int"

instance Present Integer where
  presentValue _ _ i =
    Integral (presentType (return i))
             (Constructor (show i))
  presentType _ = "Prelude.Integer"

instance Present Char where
  presentValue _ _ i =
    Char (presentType (return i))
         (show i)
  presentType _ = "Prelude.Char"

instance (Present a,Present b) => Present (a,b) where
  presentValue h (Cursor []) self@(x,y) =
    Tuple (presentType (return self))
          [(presentType (return x)
           ,h <>
            Cursor [0])
          ,(presentType (return y)
           ,h <>
            Cursor [1])]
  presentValue h (Cursor (0:j)) (x,_) =
    presentValue h (Cursor j) x
  presentValue h (Cursor (_:j)) (_,y) =
    presentValue h (Cursor j) y
  presentType p = "(" <> presentType px <> "," <> presentType py <> ")"
    where (px,py) = proxyTuple p
          proxyTuple :: Proxy (a,b) -> (Proxy a,Proxy b)
          proxyTuple _ = (Proxy,Proxy)

instance Present a => Present [a] where
  presentValue h (Cursor (0:j)) (x:_) =
    presentValue h (Cursor j) x
  presentValue h (Cursor (_:j)) (_:xs) =
    presentValue h (Cursor j) xs
  presentValue h _ xs =
    List (presentType (return xs))
         (case xs of
            [] -> Nothing
            _ -> Just ((ty,h <> Cursor [0])
                      ,(ty,h <> Cursor [0])))
    where ty = presentType (proxyList (return xs))
          proxyList :: Proxy [a] -> Proxy a
          proxyList _ = Proxy
  presentType p = "[" <> ty <> "]"
    where ty = presentType (proxyList p)
          proxyList :: Proxy [a] -> Proxy a
          proxyList _ = Proxy
