{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Generate presentations for a data type.

module Present
  (-- * Presenting functions
   presentIt
  ,presentName
  ,presentTy
  -- * Presentation mediums
  ,toShow
  ,toPretty
  ,toTerm
  -- * Types
  ,Presentation(..)
  -- * Customization classes
  ,Present0(..)
  ,Present1(..)
  ,Present2(..)
  ,Present3(..)
  ,Present4(..)
  ,Present5(..)
  ,Present6(..))
  where

import Control.Monad.Trans.State.Strict
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Language.Haskell.TH
import Control.Monad

--------------------------------------------------------------------------------
-- Types

-- | A presentation of a data structure.
data Presentation
  = Integer String String
  | Char String String
  | Algebraic String String [Presentation]
  | Record String String [(String,Presentation)]
  | Tuple String [Presentation]
  | Primitive String
  deriving (Show)

--------------------------------------------------------------------------------
-- Top-level functions

-- | Present the given name.
presentName :: Name -> Q Exp
presentName name =
  do result <- try (reify name)
     case result of
       Nothing -> fail ("The name \"" ++ show name ++ "\" isn't in scope.")
       Just (VarI _ ty _ _) -> presentTy name ty
       _ ->
         help ["That name isn't a variable, we can only","present variables."]
  where try m =
          recover (pure Nothing)
                  (fmap Just m)

-- | Present the variable @it@, useful for GHCi use.
presentIt :: Q Exp
presentIt =
  do result <- try (reify name)
     case result of
       Nothing ->
         help ["The name \"it\" isn't in scope."
              ,""
              ,"If you're running this inside GHCi, \"it\" is the"
              ,"name used to refer to the last evaluated expression."
              ,""
              ,"If you're getting this error in GHCi, you have"
              ,"probably just started GHCi or loaded a module, or"
              ,"wrote a declaration (like \"data X = ..\"), in "
              ,"which case there is no \"it\" variable."
              ,""
              ,"If you're experimenting with present, you can"
              ,"try this:"
              ,""
              ,"    > data X = X Int Char"
              ,"    > X 123 'A'"
              ,"    > $presentIt"]
       Just (VarI _ ty _ _) ->
         presentTy name ty
       _ -> help ["The name \"it\" isn't a variable, we can only"
                 ,"present variables. This is a strange circumstance,"
                 ,"consider reporting this as a problem."]
  where try m =
          recover (pure Nothing)
                  (fmap Just m)
        name = mkName "it"

-- | Present a type.
presentTy :: Name -> Type -> Q Exp
presentTy name ty =
  do instances <- getPresentInstances
     (func,PState decls _ _ _ _) <-
       runStateT (unP (do e <- makePresenter ty ty
                          -- We do a first run without decl generation enabled,
                          -- which avoids recursive types.
                          -- Then we enable decl generation and re-generate.
                          P
                            (modify (\s ->
                                       s {pMakeDecls = True
                                         ,pTypes = []
                                         ,pTypesCache = pTypes s}))
                          _ <- makePresenter ty ty
                          return e))
                 (PState [] [] [] False instances)
     ds <- mapM (\(n,t,ex) -> makeDec n t ex) decls
     letE (valD (varP (mkName "parens"))
                (normalB [|\x -> "(" ++ (x :: String) ++ ")"|])
                [] :
           map return (concat ds))
          (appE (appE (varE 'snd)
                      (return func))
                (varE name))

-- | Get a mapping from type to instance methods of instances of
-- Present, Present1, etc.
getPresentInstances :: Q [(Name,Name)]
getPresentInstances =
  do p0 <- getFor ''Present0
     p1 <- getFor ''Present1
     p2 <- getFor ''Present2
     p3 <- getFor ''Present3
     p4 <- getFor ''Present4
     return (concat [p0,p1,p2,p3,p4])
  where getFor cls =
          do result <- reify cls
             case result of
               ClassI (ClassD _ _ _ _ [SigD method _]) instances ->
                 return (mapMaybe (\i ->
                                     case i of
                                       InstanceD _ (AppT (ConT _className) (ConT typeName)) _ ->
                                         Just (typeName,method)
                                       _ -> Nothing)
                                  instances)
               _ -> return []

--------------------------------------------------------------------------------
-- Presentation monad

-- | The presentation generating monad. Transforms over Q and spits
-- out declarations.
newtype P a = P { unP :: StateT PState Q a}
  deriving (Applicative,Monad,Functor)

-- | The presentation state.
data PState =
  PState {pDecls :: [(Name,Type,Exp)]
         ,pTypes :: [(Type,Exp)]
         ,pTypesCache :: [(Type,Exp)]
         ,pMakeDecls :: Bool
         ,pInstances :: [(Name,Name)]}

-- | Reify a name.
reifyP :: Name -> P Info
reifyP = liftQ . reify

-- | Declare a printer for the given type name, returning an
-- expression referencing that printer.
declareP :: Name -> Name -> [TyVarBndr] -> P Exp -> P Exp
declareP name tyname (map unkind -> tyvars) valueFunc =
  do st <- P get
     unless (any ((== (present_T name)) . fst3)
                 (pDecls st))
            (do valueBody <- valueFunc
                P (modify (\s ->
                             s {pDecls =
                                  ((present_T name),ty,valueBody) : pDecls s})))
     return (VarE (present_T name))
  where ty =
          ForallT (map unkind tyvars)
                  []
                  (foldr funTy
                         (presentT (foldl AppT
                                          (ConT tyname)
                                          (map toTy tyvars)))
                         (map (presentT . toTy) tyvars))
          where presentT t =
                  tupleT2 (ConT ''String)
                          (funTy t (ConT ''Presentation))
                tupleT2 a b = AppT (AppT (TupleT 2) a) b
                funTy x y = AppT (AppT ArrowT x) y
                toTy t =
                  case t of
                    PlainTV n -> VarT n
                    KindedTV n k -> SigT (VarT n) k
        fst3 (x,_,_) = x

-- | Strip out redundant kinds which unnecessarily cause
-- KindSignatures to be required.
unkind :: TyVarBndr -> TyVarBndr
unkind (KindedTV n StarT) = PlainTV n
unkind x = x

-- | An error has occured so we'll display something helpful.
help :: Monad m => [String] -> m a
help ls =
  fail (unlines (take 1 ls ++ map ("    " ++) (drop 1 (ls ++ feedback))))
  where feedback =
          [""
          ,"If you think this message was unhelpful, or that"
          ,"there is a bug in the present library, please"
          ,"file a bug report here: "
          ,""
          ,"https://github.com/chrisdone/present/issues/new"
          ,""
          ,"Your feedback will be very helpful to make this"
          ,"tool as easy to use as possible."]

--------------------------------------------------------------------------------
-- Presentation generators

-- | Make a present for a type.
makePresenter :: Type -> Type -> P Exp
makePresenter originalType ty =
  do types <- P (gets pTypes)
     case lookup ty types of
       Nothing ->
         do e <- go
            P (modify (\s -> s { pTypes = (ty,e) : pTypes s}))
            return e
       Just e -> return e
  where go = case ty of
               AppT f a ->
                 AppE <$> (makePresenter originalType f)
                      <*> (makePresenter originalType a)
               ConT name -> do makeDecls <- P (gets pMakeDecls)
                               if makeDecls
                                  then makeConPresenter originalType name
                                  else return (VarE (present_T name))
               ForallT _vars _ctx ty' ->
                 makePresenter originalType ty'
               SigT _ _ -> error ("Unsupported type: " ++ pprint ty ++ " (SigT)")
               VarT _ ->
                 help ["Cannot present this type variable"
                      ,""
                      ,"    " ++ pprint ty
                      ,""
                      ,"from the type we're trying to present: "
                      ,""
                      ,"    " ++ pprint originalType
                      ,""
                      ,"Type variables present an ambiguity: we don't know"
                      ,"what to print for them. If your type is like this:"
                      ,""
                      ,"    Maybe a"
                      ,""
                      ,"You can try instead adding a type annotation to your"
                      ,"expression so that there are no type variables,"
                      ,"like this:"
                      ,""
                      ,"    > let it = Nothing :: Maybe ()"
                      ,"    > $presentIt"]
               PromotedT _ -> error ("Unsupported type: " ++ pprint ty ++ " (PromotedT)")
               TupleT arity -> makeTuplePresenter originalType arity
               UnboxedTupleT _ ->
                 error ("Unsupported type: " ++ pprint ty ++ " (UnboxedTupleT)")
               ArrowT -> error ("Unsupported type: " ++ pprint ty ++ " (ArrowT)")
               EqualityT -> error ("Unsupported type: " ++ pprint ty ++ " (EqualityT)")
               ListT -> error ("Unsupported type: " ++ pprint ty ++ " (ListT)")
               PromotedTupleT _ ->
                 error ("Unsupported type: " ++ pprint ty ++ " (PromotedTupleT)")
               PromotedNilT ->
                 error ("Unsupported type: " ++ pprint ty ++ " (PromotedNilT)")
               PromotedConsT ->
                 error ("Unsupported type: " ++ pprint ty ++ " (PromotedConsT)")
               StarT -> error ("Unsupported type: " ++ pprint ty ++ " (StarT)")
               ConstraintT ->
                 error ("Unsupported type: " ++ pprint ty ++ " (ConstraintT)")
               LitT _ -> error ("Unsupported type: " ++ pprint ty ++ " (LitT)")

-- | Make a declaration given the name and type.
makeDec :: Name -> Type -> Exp -> Q [Dec]
makeDec name ty e =
  return [SigD name ty
         ,ValD (VarP name)
               (NormalB e)
               []]

-- | Make a tuple presenter.
makeTuplePresenter :: Type -> Int -> P Exp
makeTuplePresenter _originalType_ arity =
  declareP (mkName ("Tuple" ++ show arity))
           (mkName ("(" ++
                    intercalate
                      ","
                      (map (const "")
                           ([1 .. arity])) ++
                    ")"))
           (map (PlainTV . slot_X)
                [1 .. arity])
           (liftQ (parensE (foldl (\inner a -> lamE [varP a] inner)
                                  [|let typePrinter =
                                          ("(" ++
                                           intercalate
                                             ","
                                             ($(listE (map (\i ->
                                                              appE (varE 'fst)
                                                                   (varE i))
                                                           printers))) ++
                                           ")")
                                    in (typePrinter
                                       ,$(lamE [tupP (map (varP . slot_X)
                                                          [1 .. arity])]
                                               [|(Tuple typePrinter
                                                        $(listE (map (\i ->
                                                                        appE (appE (varE 'snd)
                                                                                   (varE (makePrinterI i)))
                                                                             (varE (slot_X i)))
                                                                     [1 .. arity])))|]))|]
                                  (reverse printers))))
  where printers = map makePrinterI [1 .. arity]
        makePrinterI = present_X . mkName . show

-- | Make a constructor presenter.
makeConPresenter :: Type -> Name -> P Exp
makeConPresenter originalType thisName =
  do info <- reifyP thisName
     case info of
       TyConI dec ->
         case dec of
           DataD _ctx typeName typeVariables constructors _names ->
             case lookup typeName builtInPresenters of
               Just presentE ->
                 declareP typeName typeName typeVariables (liftQ presentE)
               Nothing ->
                 do instances <- P (gets pInstances)
                    case lookup typeName instances of
                      Just method ->
                        declareP typeName typeName typeVariables (liftQ (varE method))
                      Nothing ->
                        declareP typeName typeName
                                 typeVariables
                                 (makeDataD originalType typeVariables typeName constructors)
           TySynD _name _typeVariables _ty ->
             error "Type synonyms aren't supported."
           x ->
             error ("Unsupported type declaration: " ++
                    pprint x ++ " (" ++ show x ++ ")")
       PrimTyConI name _arity _unlifted ->
         pure (ParensE (LamE [WildP]
                             (AppE (ConE 'Primitive)
                                   (nameE name))))
       _ -> error ("Unsupported type for presenting: " ++ show thisName)

-- | Make a printer for a data declaration.
makeDataD :: Type -> [TyVarBndr] -> Name -> [Con] -> P Exp
makeDataD originalType typeVariables typeName constructors =
  foldl wrapInArg lamBody (reverse typeVariables)
  where thisType = mkName "thisType"
        lamBody =
          do tyE <- typePrinter
             lcase <- caseOnConstructors
             return (LetE [ValD (VarP thisType)
                                (NormalB tyE)
                                []]
                          (TupE [VarE thisType,lcase]))
        typePrinter =
          liftQ [|$(varE (mkName "parens"))
                    (unwords ($(stringE (show typeName)) :
                            $(listE (map (\i ->
                                            appE (varE 'fst)
                                                 (varE (present_X (typeVariableName i))))
                                         typeVariables))))|]
        wrapInArg body i =
          ParensE <$> (LamE [VarP (present_X (typeVariableName i))] <$> body)
        caseOnConstructors = LamCaseE <$> (mapM constructorCase constructors)
        constructorCase con =
          case con of
            NormalC name slots -> normalConstructor name slots
            InfixC slot1 name slot2 ->
              normalConstructor name
                                [slot1,slot2]
            _ ->
              case con of
                NormalC _ _ -> error ("NormalC")
                RecC _ _ -> error ("RecC")
                InfixC _ _ _ -> error ("InfixC")
                ForallC _ _ _ -> error ("ForallC")
        normalConstructor name slots =
          Match constructorPattern <$> matchBody <*> pure []
          where constructorPattern =
                  (ConP name
                        (map (VarP . slot_X . fst)
                             (zip [1 ..] slots)))
                matchBody =
                  (NormalB <$>
                   (AppE (AppE (AppE (ConE 'Algebraic)
                                     (VarE thisType))
                               (nameE name)) <$>
                    (ListE <$> mapM constructorSlot (zip [1 ..] slots))))
        constructorSlot (i,(_bang,typ)) =
          AppE <$> fmap (AppE (VarE 'snd)) (express typ) <*> pure (VarE (slot_X i))
          where express (VarT appliedTyVar) =
                  return (VarE (present_X appliedTyVar))
                express (AppT f x) = AppE <$> express f <*> express x
                express ty@ConT{} =
                  do P (modify (\s -> s {pTypes = pTypesCache s}))
                     e <- makePresenter originalType ty
                     P (modify (\s -> s {pTypes = []}))
                     return e
                express ty =
                  help ["Unsupported type: " ++
                        pprint ty ++ " (" ++ show ty ++ ")"]

--------------------------------------------------------------------------------
-- Name generators

-- | Given the name of a type Foo, make a function name like p_Foo.
present_T :: Name -> Name
present_T name = mkName ("present_" ++ concatMap normalize (show name))
  where normalize c =
          case c of
            '_' -> "__"
            '.' -> "_"
            _ -> [c]

-- | Make a variable name for presenting a constructor slot X.
slot_X :: Int -> Name
slot_X = mkName . ("slot_" ++) . show

-- | Make a variable name for presenting a type variable X.
present_X :: Name -> Name
present_X i = mkName ("present_" ++ show i)

--------------------------------------------------------------------------------
-- TH extras

-- | Get the name of a type variable.
typeVariableName :: TyVarBndr -> Name
typeVariableName (PlainTV name) = name
typeVariableName (KindedTV name _) = name

-- | Make a string expression from a name.
nameE :: Name -> Exp
nameE = LitE . StringL . show

-- | Our specific lifter.
liftQ :: Q a -> P a
liftQ m =
  P (StateT (\s ->
               do v <- m
                  return (v,s)))

--------------------------------------------------------------------------------
-- Built-in custom printers

-- | Printers for built-in data types with custom representations
-- (think: primitives, tuples, etc.)
builtInPresenters :: [(Name,Q Exp)]
builtInPresenters = concat [integerPrinters,charPrinters]
  where charPrinters = map makeCharPrinter [''Char]
          where makeCharPrinter name =
                  (name,[|("Prelude.Char",Char "Prelude.Char" . return)|])
        integerPrinters =
          map makeIntPrinter
              [''Integer
              ,''Int
              ,''Int8
              ,''Int16
              ,''Int32
              ,''Int64
              ,''Word
              ,''Word8
              ,''Word32
              ,''Word64]
          where makeIntPrinter name =
                  (name,[|($(stringE (show name)),Integer $(stringE (show name)) . show)|])

--------------------------------------------------------------------------------
-- Extension classes

class Present0 a where
  present0 :: a -> Presentation

class Present1 a where
  present1
    :: (x -> Presentation) -> a x -> Presentation

class Present2 a where
  present2
    :: (x -> Presentation) -> (y -> Presentation) -> a x y -> Presentation

class Present3 a where
  present3 :: (x -> Presentation)
           -> (y -> Presentation)
           -> (z -> Presentation)
           -> a x y z
           -> Presentation

class Present4 a where
  present4 :: (x -> Presentation)
           -> (y -> Presentation)
           -> (z -> Presentation)
           -> (z0 -> Presentation)
           -> a x y z z0
           -> Presentation

class Present5 a where
  present5 :: (x -> Presentation)
           -> (y -> Presentation)
           -> (z -> Presentation)
           -> (z0 -> Presentation)
           -> (z1 -> Presentation)
           -> a x y z z0 z1
           -> Presentation

class Present6 a where
  present6 :: (x -> Presentation)
           -> (y -> Presentation)
           -> (z -> Presentation)
           -> (z0 -> Presentation)
           -> (z1 -> Presentation)
           -> (z2 -> Presentation)
           -> a x y z z0 z1 z2
           -> Presentation

--------------------------------------------------------------------------------
-- Presentation mediums

-- | To a familiar Show-like string.
toShow :: Presentation -> String
toShow =
  \case
    Integer _ i -> i
    Char _ c -> "'" ++ c ++ "'"
    Algebraic _type name slots ->
      name ++
      (if null slots
          then ""
          else " ") ++
      intercalate " "
                  (map recur slots)
    Record _type name fields ->
      name ++
      " " ++
      intercalate ","
                  (map showField fields)
      where showField (fname,slot) = fname ++ " = " ++ recur slot
    Tuple _type slots ->
      "(" ++
      intercalate ","
                  (map recur slots) ++
      ")"
    Primitive p -> p
  where recur p | atomic p = toShow p
                | otherwise = "(" ++ toShow p ++ ")"
          where atomic = \case
                            Integer{} -> True
                            Char{} -> True
                            Tuple{} -> True
                            _ -> False

-- | Pretty print the presentation.
toPretty :: Presentation -> String
toPretty =
  \case
    Integer _ _ -> undefined
    Char _ _ -> undefined
    Algebraic _ _ _ -> undefined
    Record _ _ _ -> undefined
    Tuple _ _ -> undefined
    Primitive _ -> undefined

-- | A terminal presentation.
toTerm :: Presentation -> String
toTerm =
  \case
    Integer _ _ -> undefined
    Char _ _ -> undefined
    Algebraic _ _ _ -> undefined
    Record _ _ _ -> undefined
    Tuple _ _ -> undefined
    Primitive _ -> undefined
