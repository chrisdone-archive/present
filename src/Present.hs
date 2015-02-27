{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Structured, lazy presentations of Haskell data types for
-- inspection while developing.

module Present where

import           Data.Monoid (Monoid)
import           Language.Haskell.TH (Q)

import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.String as String
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Lift as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Printf as Printf

--------------------------------------------------------------------------------
-- Types

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
  Ident !TH.PkgName
        !TH.ModName
        !TH.OccName
  deriving (Show)

instance TH.Lift Ident where
  lift (Ident (TH.PkgName pkg) (TH.ModName md) (TH.OccName occ)) =
    [|Ident (TH.PkgName pkg) (TH.ModName md) (TH.OccName occ)|]

-- | Type.
data Ty
  = TyCon !Ident
  | TyTuple ![Ty]
  | TyList !Ty
  | TyApp !Ty
          !Ty
  deriving (Show)

--------------------------------------------------------------------------------
-- Template Haskell code generation

-- | Make a presentation at the given cursor.
present :: Mode -> Cursor -> TH.Name -> Q TH.Exp
present mode cursor name =
  do info <- TH.reify name
     case info of
       TH.VarI _ ty _  _ -> generate ty
       _ -> error "present: Expects a variable name as argument."

-- | Generate presenters for constructors of a given type.
generate :: TH.Type -> Q TH.Exp
generate = undefined

--------------------------------------------------------------------------------
-- Conversion to s-expressions

-- | Convert from a presentation to an s-expression.
fromPresentation :: Presentation -> Sexp
fromPresentation p =
  case p of
    Integral ty i ->
      plist [("tag","integral"),
               ("type",fromType ty),
               ("integer",SInteger i)]
    Ratio ty n d ->
      plist [("tag","ratio"),
             ("type",fromType ty),("numerator",SInteger n)
            ,("denominator",SInteger d)]
    Decimal ty s ->
      plist [("tag","decimal"),
             ("type",fromType ty),("decimal",SString s)]
    Char ty ch ->
      plist [("tag","char"),
             ("type",fromType ty),
             ("char",SChar ch)]
    String ty mcons ->
      plist
      (concat [[("tag","string")
               ,("type",fromType ty)]
              ,[("car",fromCursor car)|Just (car,_) <- [mcons]]
              ,[("cdr",fromCursor cdr)|Just (_,cdr) <- [mcons]]])
    Tuple ty slots ->
      plist [("tag","tuple")
            ,("type",fromType ty)
            ,("slots",SList (map fromCursor slots))]
    List ty mcons ->
      plist
      (concat [[("tag","list")
               ,("type",fromType ty)]
              ,[("car",fromCursor car)|Just (car,_) <- [mcons]]
              ,[("cdr",fromCursor cdr)|Just (_,cdr) <- [mcons]]])
    Vector ty slots ->
      plist [("tag","vector")
            ,("type",fromType ty)
            ,("slots",SList (map fromCursor slots))]
    Alg fixity ty name slots ->
      plist [("tag","alg")
            ,("fixity",case fixity of
                         PrefixCons -> SSymbol "prefix"
                         InfixCons  -> SSymbol "infix")
            ,("type",fromType ty)
            ,("name",fromIdent name)
            ,("slots",SList (map fromCursor slots))]
    Record ty name slots ->
      plist [("tag","record")
            ,("type",fromType ty)
            ,("name",fromIdent name)
            ,("slots",
              SList (map (\(field,cursor) ->
                             plist [("tag","field")
                                   ,("name",fromIdent field)
                                   ,("cursor",fromCursor cursor)])
                          slots))]
    Bottom ty msg ->
      plist [("tag","bottom")
            ,("type",fromType ty)
            ,("msg",SString msg)]

-- | Convert from a type to s-expression representation.
fromType :: Ty -> Sexp
fromType ty =
  plist
    (case ty of
       TyCon name ->
         [("tag","con")
         ,("name",fromIdent name)]
       TyTuple tys ->
         [("tag","tuple")
         ,("slots",SList (map fromType tys))]
       TyList t ->
         [("tag","list")
         ,("type",fromType t)]
       TyApp a b ->
         [("tag","app")
         ,("op",fromType a)
         ,("arg",fromType b)])

-- | Convert from a name to s-expression representation.
fromIdent :: Ident -> Sexp
fromIdent (Ident (TH.PkgName pkg) (TH.ModName mn) (TH.OccName occ)) =
  plist [("tag","name")
        ,("pkg",SString pkg)
        ,("mod",SString mn)
        ,("occ",SString occ)]

-- | Convert from a cursor to s-expression representation.
fromCursor :: (Ty,Cursor) -> Sexp
fromCursor (ty,Cursor items) =
  plist [("type",fromType ty)
        ,("cursor",SList (map (SInteger . fromIntegral) items))]

--------------------------------------------------------------------------------
-- Simple s-expression data structure

-- | An s-expression.
data Sexp
  = SSymbol String
  | SString String
  | SInteger Integer
  | SChar Char
  | SList [Sexp]

instance String.IsString Sexp where
  fromString = SSymbol

-- | Prefix list.
plist :: [(String,Sexp)] -> Sexp
plist =
  SList .
  concatMap (\(key,val) ->
               [SSymbol (":" ++ key),val])

-- | Association list.
alist :: [(String,Sexp)] -> Sexp
alist =
  SList .
  map (\(key,val) -> SList [SSymbol key,val])

-- | Print an s-expression to a string.
toString :: Sexp -> String
toString (SInteger i) = show i
toString (SChar i) = show (fromEnum i)
toString (SSymbol s) = concatMap normalize s
  where normalize c
          | c == '-' || c == '.' || c == ':' =
            [c]
          | Char.isAlphaNum c = [c]
          | otherwise =
            "\\" ++
            [c]
toString (SString s) =
  "\"" ++
  concatMap normalize s ++
  "\""
  where normalize '"' = "\\\""
        normalize '\n' = "\\n"
        normalize c
          | Char.isPrint c = [c]
          | otherwise =
            "\\u" ++
            Printf.printf "%04d" (fromEnum c)
toString (SList s) =
  "(" ++
  L.intercalate " "
                (map toString s) ++
  ")"
