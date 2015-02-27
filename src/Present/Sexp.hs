{-# LANGUAGE OverloadedStrings #-}

-- | Conversion to s-expressions.

module Present.Sexp where

import           Data.Sexp (Sexp)
import qualified Data.Sexp as S
import           Language.Haskell.TH.Syntax
import           Present.Types

-- | Convert from a presentation to an s-expression.
fromPresentation :: Presentation -> Sexp
fromPresentation p =
  case p of
    Integral ty i ->
      S.plist [("tag","integral"),
               ("type",fromType ty),
               ("integer",S.Integer i)]
    Ratio ty n d ->
      S.plist [("tag","ratio"),
               ("type",fromType ty),("numerator",S.Integer n)
              ,("denominator",S.Integer d)]
    Decimal ty s ->
      S.plist [("tag","decimal"),
               ("type",fromType ty),("decimal",S.String s)]
    Char ty ch ->
      S.plist [("tag","char"),
               ("type",fromType ty),
               ("char",S.Char ch)]
    String ty mcons ->
      S.plist
        (concat [[("tag","string")
                 ,("type",fromType ty)]
                ,[("car",fromCursor car)|Just (car,_) <- [mcons]]
                ,[("cdr",fromCursor cdr)|Just (_,cdr) <- [mcons]]])
    Tuple ty slots ->
      S.plist [("tag","tuple")
              ,("type",fromType ty)
              ,("slots",S.List (map fromCursor slots))]
    List ty mcons ->
      S.plist
        (concat [[("tag","list")
                 ,("type",fromType ty)]
                ,[("car",fromCursor car)|Just (car,_) <- [mcons]]
                ,[("cdr",fromCursor cdr)|Just (_,cdr) <- [mcons]]])
    Vector ty slots ->
      S.plist [("tag","vector")
              ,("type",fromType ty)
              ,("slots",S.List (map fromCursor slots))]
    Alg fixity ty name slots ->
      S.plist [("tag","alg")
              ,("fixity",case fixity of
                           PrefixCons -> S.Symbol "prefix"
                           InfixCons  -> S.Symbol "infix")
              ,("type",fromType ty)
              ,("name",fromIdent name)
              ,("slots",S.List (map fromCursor slots))]
    Record ty name slots ->
      S.plist [("tag","record")
              ,("type",fromType ty)
              ,("name",fromIdent name)
              ,("slots",
                S.List (map (\(field,cursor) ->
                               S.plist [("tag","field")
                                       ,("name",fromIdent field)
                                       ,("cursor",fromCursor cursor)])
                            slots))]
    Bottom ty msg ->
      S.plist [("tag","bottom")
              ,("type",fromType ty)
              ,("msg",S.String msg)]

-- | Convert from a type to s-expression representation.
fromType :: Ty -> Sexp
fromType ty =
  S.plist
    (case ty of
       TyCon name ->
         [("tag","con")
         ,("name",fromIdent name)]
       TyTuple tys ->
         [("tag","tuple")
         ,("slots",S.List (map fromType tys))]
       TyList t ->
         [("tag","list")
         ,("type",fromType t)]
       TyApp a b ->
         [("tag","app")
         ,("op",fromType a)
         ,("arg",fromType b)])

-- | Convert from a name to s-expression representation.
fromIdent :: Ident -> Sexp
fromIdent (Ident (PkgName pkg) (ModName mn) (OccName occ)) =
  S.plist [("tag","name")
          ,("pkg",S.String pkg)
          ,("mod",S.String mn)
          ,("occ",S.String occ)]

-- | Convert from a cursor to s-expression representation.
fromCursor :: (Ty,Cursor) -> Sexp
fromCursor (ty,Cursor items) =
  S.plist [("type",fromType ty)
          ,("cursor",S.List (map (S.Integer . fromIntegral) items))]
