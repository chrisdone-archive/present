{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
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

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Data
import Data.Int
import Data.List
import Data.Maybe
import Data.String
import Data.Word
import Language.Haskell.TH
import Text.Printf

--------------------------------------------------------------------------------
-- Types

-- | A presentation of a data structure.
data Presentation
  = Integer String
            String -- ^ Integral types.
  | Char String
         String -- ^ Character types.
  | Algebraic String
              String
              [Presentation] -- ^ Algebraic data structures.
  | Record String
           String
           [(String,Presentation)] -- ^ Record types with fields.
  | Tuple String
          [Presentation] -- ^ Tuples.
  | List String
         [Presentation] -- ^ Lists or list-like things.
  | String String
           String -- ^ Strings or string-like things.
  | Primitive String -- ^ Primitive type that can't be presented.
  | Function String -- ^ Function which cannot be presented.
  deriving ((Show))

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
declareP :: Name -> (Name,[TyVarBndr]) -> P Exp -> P Exp
declareP name etyname valueFunc =
  do st <- P get
     unless (any ((== (present_T name)) . fst3)
                 (pDecls st))
            (do valueBody <- valueFunc
                P (modify (\s ->
                             s {pDecls =
                                  ((present_T name),ty,valueBody) : pDecls s})))
     return (VarE (present_T name))
  where ty =
          case etyname of
            (tyname,(map unkind -> tyvars)) ->
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
            P (modify (\s -> s {pTypes = (ty,e) : pTypes s}))
            return e
       Just e -> return e
  where go =
          case ty of
            AppT op a ->
              do let regular =
                       AppE <$> (makePresenter originalType op) <*>
                       (makePresenter originalType a)
                 let (f,args) = collapseApp ty
                     substitute name =
                       do fname <- reifyP name
                          case fname of
                            TyConI (TySynD _name vars synty) ->
                              let appliedTy =
                                    applyTypeSubstitution (zip vars args)
                                                          synty
                              in makePresenter appliedTy appliedTy
                            _ -> regular
                 case f of
                   ConT name -> substitute name
                   ListT -> substitute (mkName "[]")
                   ArrowT -> makeFunctionPrinter ty
                   _ -> regular
            ConT name ->
              do makeDecls <- P (gets pMakeDecls)
                 do i <- reifyP name
                    case i of
                      TyConI (TySynD _ _ realType) ->
                        makePresenter realType realType
                      _ ->
                        if makeDecls
                           then makeConPresenter originalType name
                           else return (VarE (present_T name))
            ForallT _vars ctxs ambiguousType ->
              do unambiguousType <-
                   foldM (\wipType ctx ->
                            case ctx of
                              AppT (ConT className) (VarT var) ->
                                case lookup className defaultedClasses of
                                  Nothing -> return wipType
                                  Just getSubstitution ->
                                    do sub <- liftQ getSubstitution
                                       return (applyTypeSubstitution
                                                 [(PlainTV var,sub)]
                                                 wipType)
                              _ -> return wipType)
                         ambiguousType
                         ctxs
                 makePresenter originalType unambiguousType
            TupleT arity -> makeTuplePresenter originalType arity
            ListT -> makeListPresenter originalType
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
            PromotedT _ ->
              error ("Unsupported type: " ++ pprint ty ++ " (PromotedT)")
            SigT _ _ -> error ("Unsupported type: " ++ pprint ty ++ " (SigT)")
            UnboxedTupleT _ ->
              error ("Unsupported type: " ++ pprint ty ++ " (UnboxedTupleT)")
            ArrowT -> error ("Unsupported type: " ++ pprint ty ++ " (ArrowT)")
            EqualityT ->
              error ("Unsupported type: " ++ pprint ty ++ " (EqualityT)")
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

-- | Make a presenter for functions.
makeFunctionPrinter :: Type -> P Exp
makeFunctionPrinter originalType =
  liftQ [|($(stringE (pprint originalType))
          ,\_ -> Function $(stringE (pprint originalType)))|]

-- | Make a presenter for lists.
makeListPresenter :: Type -> P Exp
makeListPresenter _originalType =
  do declareP (mkName "List")
              (''[],[PlainTV (slot_X 1)])
              (liftQ [|\present_a ->
                         let ty = "[" ++ fst present_a ++ "]"
                         in (ty
                            ,\xs ->
                               case fst present_a of
                                 "Prelude.Char" ->
                                   String "String" (concatMap getCh (map (snd present_a) xs))
                                   where getCh (Char "Prelude.Char" ch) = ch
                                         getCh _ = []
                                 _ -> List ty (map (snd present_a) xs))|])

-- | Make a tuple presenter.
makeTuplePresenter :: Type -> Int -> P Exp
makeTuplePresenter _originalType_ arity =
  declareP (mkName ("Tuple" ++ show arity))
           (mkName ("(" ++
                    intercalate
                      ","
                      (map (const "")
                           ([1 .. arity])) ++
                    ")")
           ,(map (PlainTV . slot_X)
                 [1 .. arity]))
           (liftQ (parensE (foldl (\inner a -> lamE [varP a] inner)
                                  [|let typePrinter
                                          :: String
                                        typePrinter =
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
             dataType typeName typeVariables constructors
           NewtypeD _ typeName typeVariables constructor _names ->
             dataType typeName typeVariables [constructor]
           x ->
             error ("Unsupported type declaration: " ++
                    pprint x ++
                    " (" ++ show x ++ ") (" ++ show originalType ++ ")")
       PrimTyConI name arity _unlifted ->
         liftQ (foldl (\inner next -> parensE (lamE [wildP] inner))
                      [|($(stringE (show name))
                        ,\_ -> Primitive ("<" ++ $(stringE (show name)) ++ ">"))|]
                      [1..arity])
       _ -> error ("Unsupported type for presenting: " ++ show thisName)
  where dataType typeName typeVariables constructors =
          do instances <- P (gets pInstances)
             case lookup typeName instances of
               Just method ->
                 declareP typeName
                          (typeName,typeVariables)
                          (liftQ (varE method))
               Nothing ->
                 case lookup typeName builtInPresenters of
                   Just presentE ->
                     declareP typeName
                              (typeName,typeVariables)
                              (liftQ presentE)
                   Nothing ->
                     declareP typeName
                              (typeName,typeVariables)
                              (makeDataD originalType typeVariables typeName constructors)

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
            NormalC name slots ->
              makeConstructor 'Algebraic
                              name
                              (map (Nothing,) slots)
            InfixC slot1 name slot2 ->
              makeConstructor 'Algebraic
                              name
                              [(Nothing,slot1),(Nothing,slot2)]
            RecC name fields ->
              makeConstructor
                'Record
                name
                (map (\(fname,strict,typ) -> (Just fname,(strict,typ))) fields)
            _ ->
              case con of
                NormalC _ _ -> error ("NormalC")
                RecC _ _ -> error ("RecC")
                InfixC _ _ _ -> error ("InfixC")
                ForallC _ _ _ -> error ("ForallC")
        makeConstructor presentationCons name slots =
          Match constructorPattern <$> matchBody <*> pure []
          where constructorPattern =
                  (ConP name
                        (map (VarP . slot_X . fst)
                             (zip [1 ..] slots)))
                matchBody =
                  (NormalB <$>
                   (AppE (AppE (AppE (ConE presentationCons)
                                     (VarE thisType))
                               (nameE name)) <$>
                    (ListE <$> mapM constructorSlot (zip [1 ..] slots))))
        constructorSlot (i,(mfieldName,(_bang,typ))) =
          do presentation <- makePresentation
             return (case mfieldName of
                       Just name ->
                         TupE [LitE (StringL (show (name :: Name)))
                              ,presentation]
                       Nothing -> presentation)
          where makePresentation =
                  do let (f,args) = collapseApp typ
                     AppE <$>
                       fmap (AppE (VarE 'snd))
                            (case f of
                               ArrowT -> makeFunctionPrinter typ
                               _ -> express typ) <*>
                       pure (VarE (slot_X i))
                express (VarT appliedTyVar) =
                  return (VarE (present_X appliedTyVar))
                express (AppT f x) = AppE <$> express f <*> express x
                express (TupleT arity) = makeTuplePresenter typ arity
                express ListT = makeListPresenter typ
                express ty@ConT{} =
                  do P (modify (\s -> s {pTypes = pTypesCache s}))
                     e <- makePresenter originalType ty
                     P (modify (\s -> s {pTypes = []}))
                     return e
                express ty =
                  help ["Unsupported type: "
                       ,"originalType: " ++ show originalType
                       ,"typ: " ++ show typ
                       ,"ty: " ++ show ty]

--------------------------------------------------------------------------------
-- Common type manipulation operations

-- | Apply the given substitutions to the type.
applyTypeSubstitution :: [(TyVarBndr,Type)] -> Type -> Type
applyTypeSubstitution subs = go
  where go =
          \case
            ForallT vars ctx ty -> ForallT vars ctx (go ty)
            AppT f x ->
              AppT (go f)
                   (go x)
            SigT ty k -> SigT (go ty) k
            VarT a
              | Just (_,b) <- find ((== a) . typeVariableName . fst) subs -> b
              | otherwise -> VarT a
            x -> x

-- | Collapse a series of App (App (App f) y) z into (f,[y,z])
collapseApp :: Type -> (Type,[Type])
collapseApp = go []
  where go args (AppT f x) = go (x : args) f
        go args f          = (f,args)

--------------------------------------------------------------------------------
-- Name generators

-- | Given the name of a type Foo, make a function name like p_Foo.
present_T :: Name -> Name
present_T name = mkName ("present_" ++ concatMap normalize (show name))
  where normalize c =
          if isAlphaNum c
             then [c]
             else if isSpace c
                     then ""
                     else printf "_%x" (ord c)

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

-- | Classes which when encountered in a forall context should have
-- their corresponding type variables substituted on the right hand
-- side with the given type.
defaultedClasses :: [(Name,Q Type)]
defaultedClasses =
  [(''Integral,[t|Integer|])
  ,(''Num,[t|Integer|])
  ,(''Data,[t|()|])
  ,(''Bounded,[t|()|])
  ,(''Ord,[t|()|])
  ,(''Eq,[t|()|])
  ,(''Read,[t|()|])
  ,(''Show,[t|()|])
  ,(''IsString,[t|String|])]

-- | Printers for built-in data types with custom representations
-- (think: primitives, tuples, etc.)
builtInPresenters :: [(Name,Q Exp)]
builtInPresenters = concat [integerPrinters,charPrinters]
  where charPrinters = map makeCharPrinter [''Char]
          where makeCharPrinter name =
                  (name,[|("Prelude.Char",Char "Prelude.Char" . return)|])
        integerPrinters =
          map makeIntPrinter
              [''Integer ,''Int ,''Int8 ,''Int16 ,''Int32 ,''Int64 ,''Word ,''Word8 ,''Word32 ,''Word64]
          where makeIntPrinter name =
                  (name,[|($(stringE (show name)),Integer $(stringE (show name)) . show)|])

--------------------------------------------------------------------------------
-- Extension classes

class Present0 a where
  present0
    :: (String,a -> Presentation)

class Present1 a where
  present1
    :: (String,x -> Presentation)
    -> (String,a x -> Presentation)

class Present2 a where
  present2
    :: (String,x -> Presentation)
    -> (String,y -> Presentation)
    -> (String,a x y -> Presentation)

class Present3 a where
  present3
    :: (String,x -> Presentation)
    -> (String,y -> Presentation)
    -> (String,z -> Presentation)
    -> (String,a x y z -> Presentation)

class Present4 a where
  present4
    :: (String,x -> Presentation)
    -> (String,y -> Presentation)
    -> (String,z -> Presentation)
    -> (String,z0 -> Presentation)
    -> (String,a x y z z0 -> Presentation)

class Present5 a where
  present5
    :: (String,x -> Presentation)
    -> (String,y -> Presentation)
    -> (String,z -> Presentation)
    -> (String,z0 -> Presentation)
    -> (String,z1 -> Presentation)
    -> (String,a x y z z0 z1 -> Presentation)

class Present6 a where
  present6
    :: (String,x -> Presentation)
    -> (String,y -> Presentation)
    -> (String,z -> Presentation)
    -> (String,z0 -> Presentation)
    -> (String,z1 -> Presentation)
    -> (String,z2 -> Presentation)
    -> (String,a x y z z0 z1 z2 -> Presentation)

--------------------------------------------------------------------------------
-- Presentation mediums

-- | To a familiar Show-like string.
toShow :: Presentation -> String
toShow =
  \case
    Integer _ i -> i
    Char _ c -> "'" ++ c ++ "'"
    Function ty -> "<" ++ ty ++ ">"
    Algebraic _type name slots ->
      name ++
      (if null slots
          then ""
          else " ") ++
      intercalate " "
                  (map recur slots)
    Record _type name fields ->
      name ++
      " {" ++
      intercalate ","
                  (map showField fields) ++
      "}"
      where showField (fname,slot) = fname ++ " = " ++ recur slot
    Tuple _type slots ->
      "(" ++
      intercalate ","
                  (map toShow slots) ++
      ")"
    List _type slots ->
      "[" ++
      intercalate ","
                  (map recur slots) ++
      "]"
    Primitive p -> p
    String _ string -> show string
  where recur p
          | atomic p = toShow p
          | otherwise = "(" ++ toShow p ++ ")"
          where atomic =
                  \case
                    List{} -> True
                    Integer{} -> True
                    Char{} -> True
                    Tuple{} -> True
                    Record{} -> True
                    String{} -> True
                    _ -> False
