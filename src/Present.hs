{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Structured, lazy presentations of Haskell data types for
-- inspection while developing.

module Present where

import           Data.Int
import           Data.Monoid (Monoid)
import           Data.Word
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

instance TH.Lift Ty where
  lift ty =
    case ty of
      TyCon i -> [|TyCon i|]
      TyTuple tys -> [|TyTuple tys|]
      TyList t -> [|TyList t|]
      TyApp a b -> [|TyApp a b|]

-- | Just for better documentation.
type TExp a = TH.Exp

--------------------------------------------------------------------------------
-- Top-level running functions

-- | Present GHCi's "it" name.
presentIt :: Q (TExp (Mode -> Cursor -> Presentation))
presentIt =
  present (TH.mkName "it")

-- | Present GHCi's "it" name.
presentItSexp :: Q (TExp (Mode -> Cursor -> IO ()))
presentItSexp =
  [|\mode cursor ->
      putStrLn (toString (fromPresentation ($(present (TH.mkName "it")) mode cursor)))|]

--------------------------------------------------------------------------------
-- Template Haskell code generation

-- | Make a presentation at the given cursor.
present :: TH.Name -> Q (TExp (Mode -> Cursor -> a -> Presentation))
present name =
  do info <- TH.reify name
     case info of
       TH.VarI _ ty _ _ ->
         do TH.runIO (print ty)
            [|\mode cursor ->
                let hist = (Cursor [])
                in $(generate ty) mode
                                  hist
                                  cursor
                                  $(TH.varE name)|]
       _ ->
         error "Present.present: Expects a variable name as argument."

-- | Generates a function from a type.
generate :: TH.Type -> Q (TExp (Mode -> Cursor -> a -> Presentation))
generate = go []
  where go args ty =
          case ty of
            TH.ForallT _ _ ty -> go args ty
            TH.SigT _ ty -> go args ty
            TH.VarT _ -> undefined
            TH.ConT t -> genConT t
            TH.AppT TH.ListT ty -> genList ty
            TH.AppT op@TH.AppT{} arg ->
              go (arg : args) op
            TH.AppT op arg ->
              genAppT op (arg : args)
            TH.PromotedT _ ->
              error ("Present.go: Unexpected type at this point: " ++ show ty)
            TH.TupleT i
              | i == 0 -> genUnit
              | otherwise ->
                error ("Present.go: Tuple not expected here." ++ show ty)
            TH.UnboxedTupleT _ -> undefined
            TH.ArrowT -> undefined
            TH.ListT -> undefined
            TH.PromotedTupleT _ -> undefined
            TH.PromotedNilT -> undefined
            TH.PromotedConsT -> undefined
            TH.StarT -> undefined
            TH.ConstraintT -> undefined
            TH.LitT _ -> undefined

-- | Generate a presentation for the application of some types.
genAppT :: TH.Type -> [TH.Type] -> Q (TExp (Mode -> Cursor -> Cursor -> a -> Presentation))
genAppT op args =
  case op of
    TH.TupleT len
      | length args == len ->
        if null args
           then genUnit
           else genTuple args
      | otherwise ->
        error ("Present.go: Wrong number of arguments to tuple! " ++
               show (op,args))

-- | Generate a presentation for a tuple.
genTuple :: [TH.Type] -> Q (TExp (Mode -> Cursor -> Cursor -> a -> Presentation))
genTuple args =
  [|\mode (Cursor hist) (Cursor cursor) it ->
      case cursor of
        [] ->
          Tuple (TyTuple $(TH.lift (map toTy args)))
                (map (\(i,ty) ->
                        (ty
                        ,Cursor (hist ++
                                 [i])))
                     (zip [0 ..]
                          $(TH.lift (map toTy args))))
        (j:js) ->
          let hist' =
                Cursor (hist ++
                        [j])
              cursor' = Cursor js
              badCursor =
                error ("Present.genTuple: invalid cursor number: " ++ show j)
          in $(let f i =
                     TH.lamE [TH.tupP (map (\k ->
                                              if k == i
                                                 then TH.varP (TH.mkName "x")
                                                 else TH.wildP)
                                           (zipWith const [0 ..] args))]
                             (TH.varE (TH.mkName "x"))
               in TH.caseE [|j|]
                           (map (\(i,ty) ->
                                   TH.match (TH.litP (TH.integerL (fromIntegral i)))
                                            (TH.normalB
                                               [|$(generate ty) mode
                                                                hist'
                                                                cursor'
                                                                ($(f i) it)|])
                                            [])
                                (zip [0 ..] args) ++
                            [TH.match TH.wildP (TH.normalB [|badCursor|]) []]))|]

-- | Generate a presentation for a tuple.
genList :: TH.Type -> Q (TExp (Mode -> Cursor -> Cursor -> a -> Presentation))
genList arg =
  [|let self =
          \mode (Cursor hist) (Cursor cursor) it ->
            let cursorDisplay = hist ++ cursor
                badList = error ("Present.genList: unexpected empty list for cursor: " ++
                                 show cursorDisplay)
            in case cursor of
                 [] ->
                   List $(TH.lift (toTy arg))
                        (case it of
                           [] -> Nothing
                           (_:_) ->
                             (Just (($(TH.lift (toTy arg))
                                    ,Cursor (hist ++
                                             [0]))
                                   ,($(TH.lift (TyList (toTy arg)))
                                    ,Cursor (hist ++
                                             [1])))))
                 (j:js) ->
                   let hist' =
                         Cursor (hist ++
                                 [j])
                       cursor' = Cursor js
                   in case j of
                        0 ->
                          $(generate arg)
                            mode
                            hist'
                            cursor'
                            (case it of
                               (x:_) -> x
                               _ -> badList)
                        1 ->
                          self mode
                               hist'
                               cursor'
                               (case it of
                                  (_:xs) -> xs
                                  _ -> badList)
                        _ ->
                          error ("Present.genList: invalid cursor number: " ++
                                 show cursor)
    in self|]

-- | Generate () type. This forces the tuple when presented.
genUnit :: Q (TExp (Mode -> Cursor -> a -> Presentation))
genUnit =
  [|\_mode _hist _cursor () ->
      Tuple (TyCon $(TH.lift (nameToIdent ''()))) []|]

-- | Generate a presentation for the given constructor.
genConT :: TH.Name -> Q (TExp (Mode -> Cursor -> a -> Presentation))
genConT t
  | t == ''Char =
    [|\_mode _hist _cursor ch ->
        Char (TyCon $(TH.lift (nameToIdent t))) ch|]
  | elem t
         [''Integer
         ,''Int
         ,''Int8
         ,''Int16
         ,''Int32
         ,''Int64
         ,''Word
         ,''Word8
         ,''Word16
         ,''Word32
         ,''Word64] =
    [|\_mode _hist _cursor i ->
        Integral (TyCon $(TH.lift (nameToIdent t)))
                 (fromIntegral i)|]
  | elem t [''Float,''Double,''Double] =
    [|\_mode _hist _cursor i ->
        Decimal (TyCon $(TH.lift (nameToIdent t)))
                (show i)|]
  | otherwise =
    error ("Present.go: Unhandled type constructor case: " ++ show t)

-- | Convert the wider type to the simpler presentation type.
toTy :: TH.Type -> Ty
toTy ty =
  case ty of
    TH.ForallT _ _ ty -> toTy ty
    TH.AppT a b -> TyApp (toTy a) (toTy b)
    TH.SigT ty _ -> toTy ty
    TH.VarT v -> error "Present.toTy: No type variables expected."
    TH.ConT c -> TyCon (nameToIdent c)
    TH.PromotedT c -> error "Present.toTy: No promoted types expected."
    _ -> error ("Present.toTy: Unexpected type here: " ++ show ty)

-- | Convert a name to an ident.
nameToIdent :: TH.Name -> Ident
nameToIdent (TH.Name occ flav) =
  case flav of
    TH.NameQ md -> Ident (TH.PkgName "") md occ
    TH.NameG _ pkg md -> Ident pkg md occ
    _ -> error "Unexpected name type for identifier."

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
