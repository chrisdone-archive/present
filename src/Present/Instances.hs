{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Custom instances for data types which cannot/shouldn't be derived
-- automatically.

module Present.Instances where

import Present.TH
import Present.Types

import Data.Int
import Data.Monoid
import Data.Proxy
import Data.Word

--------------------------------------------------------------------------------
-- Flat types

instance Present Char where
  presentValue _mode _ _ i =
    Char (presentType (return i))
         i
  presentType _ = ConT ''Char

instance Present () where
  presentValue _mode _ _ i =
    Tuple (presentType (return i))
          []
  presentType _ = ConT ''()

--------------------------------------------------------------------------------
-- Container types

instance (Present a,Present b) => Present (a,b) where
  presentValue _mode h (Cursor []) self@(x,y) =
    Tuple (presentType (return self))
          [(presentType (return x)
           ,h <>
            Cursor [0])
          ,(presentType (return y)
           ,h <>
            Cursor [1])]
  presentValue _mode h (Cursor (0:j)) (x,_) =
    presentValue _mode h (Cursor j) x
  presentValue _mode h (Cursor (_:j)) (_,y) =
    presentValue _mode h (Cursor j) y
  presentType p = AppT (AppT (TupleT 2)(presentType px)) (presentType py)
    where (px,py) = proxyTuple p
          proxyTuple :: Proxy (a,b) -> (Proxy a,Proxy b)
          proxyTuple _ = (Proxy,Proxy)

instance Present a => Present [a] where
  presentValue _mode h (Cursor (0:j)) (x:_) =
    presentValue _mode h (Cursor j) x
  presentValue _mode h (Cursor (_:j)) (_:xs) =
    presentValue _mode h (Cursor j) xs
  presentValue _mode h _ xs =
    List listTy
         (case xs of
            [] -> Nothing
            _ -> Just ((ty,h <> Cursor [0])
                      ,(listTy,h <> Cursor [1])))
    where listTy = (presentType (return xs))
          ty = presentType (proxyList (return xs))
          proxyList :: Proxy [a] -> Proxy a
          proxyList _ = Proxy
  presentType p = AppT ListT ty
    where ty = presentType (proxyList p)
          proxyList :: Proxy [a] -> Proxy a
          proxyList _ = Proxy

--------------------------------------------------------------------------------
-- Derivings

-- Basic Prelude sum types

$(makeGenericPresent ''Either)
$(makeGenericPresent ''Maybe)
$(makeGenericPresent ''Bool)
$(makeGenericPresent ''Ordering)

-- Integrals

$(makeIntegralPresent ''Integer)

$(makeIntegralPresent ''Int)
$(makeIntegralPresent ''Int8)
$(makeIntegralPresent ''Int16)
$(makeIntegralPresent ''Int32)
$(makeIntegralPresent ''Int64)

$(makeIntegralPresent ''Word)
$(makeIntegralPresent ''Word8)
$(makeIntegralPresent ''Word16)
$(makeIntegralPresent ''Word32)
$(makeIntegralPresent ''Word64)

-- Floating

$(makeDecimalPresent ''Float)
$(makeDecimalPresent ''Double)
