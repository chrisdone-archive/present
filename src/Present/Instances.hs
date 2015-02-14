{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Custom instances for data types which cannot/shouldn't be derived
-- automatically.

module Present.Instances where

import           Present.TH
import           Present.Types

import           Data.Monoid
import           Data.Proxy

--------------------------------------------------------------------------------
-- Flat types

instance Present Int where
  presentValue _mode _ _ i =
    Integral (presentType (return i))
             (Constructor (id (show i)))
  presentType _ = "Int"

instance Present Integer where
  presentValue _mode _ _ i =
    Integral (presentType (return i))
             (Constructor (id (show i)))
  presentType _ = "Integer"

instance Present Char where
  presentValue _mode _ _ i =
    Char (presentType (return i))
         (id (show i))
  presentType _ = "Char"

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
  presentType p = "(" <> presentType px <> "," <> presentType py <> ")"
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
  presentType p = "[" <> ty <> "]"
    where ty = presentType (proxyList p)
          proxyList :: Proxy [a] -> Proxy a
          proxyList _ = Proxy

--------------------------------------------------------------------------------
-- Derivings

$(makePresent ''Either)
