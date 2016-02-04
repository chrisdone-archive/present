{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Custom instances for data types which cannot/shouldn't be derived
-- automatically.

module Present.Instances where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Present.String as P
import           Present.TH
import           Present.Types

--------------------------------------------------------------------------------
-- Flat types

instance Present Int where
  presentValue _ _ i =
    Integral (presentType (return i))
             (Constructor (ST.pack (show i)))
  presentType _ = "Prelude.Int"

instance Present Integer where
  presentValue _ _ i =
    Integral (presentType (return i))
             (Constructor (ST.pack (show i)))
  presentType _ = "Prelude.Integer"

instance Present Char where
  presentValue _ _ i =
    Char (presentType (return i))
         (ST.pack (show i))
  presentType _ = "Prelude.Char"

--------------------------------------------------------------------------------
-- Container types

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

--------------------------------------------------------------------------------
-- Derivings

$(makePresent ''Either)
