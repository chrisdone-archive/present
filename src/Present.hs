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
   -- * Presentation accessors
  ,presentationType
  -- * Types
  ,Presentation(..)
  -- * Customization classes
  ,Present1(..)
  ,Present2(..)
  ,Present3(..)
  ,Present4(..)
  ,Present5(..)
  ,Present6(..))
  where

import Control.Monad.State.Strict
import Data.Int
import Data.Maybe
import Data.Word
import Language.Haskell.TH

--------------------------------------------------------------------------------
-- Types

-- | A presentation of a data structure.
data Presentation
  = Integer String String
  | Char String String
  | Alg String [Presentation]
  | Rec String [(String,Presentation)]
  | Tuple String [Presentation]
  | Primitive String
  deriving (Show)

-- | Get the Haskell type of the value presented.
presentationType :: Presentation -> String
presentationType =
  \case
    Integer ty _ -> ty
    Char ty _ -> ty
    Alg ty _ -> ty
    Rec ty _ -> ty
    Tuple ty _ -> ty
    Primitive ty -> ty

--------------------------------------------------------------------------------
-- Top-level functions

-- | Present the given name.
presentName :: Name -> Q Exp
presentName name =
  do result <- try (reify name)
     case result of
       Nothing -> fail ("The name \"" ++ show name ++ "\" isn't in scope.")
       Just (VarI _ ty _ _) ->
         presentTy name ty
       _ -> help ["That name isn't a variable, we can only"
                 ,"present variables."]
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
     letE (map return (concat ds))
          (appE (return func)
                (varE name))

-- | Get a mapping from type to instance methods of instances of
-- Present, Present1, etc.
getPresentInstances :: Q [(Name,Name)]
getPresentInstances =
  do p <- getFor ''Present
     p1 <- getFor ''Present1
     p2 <- getFor ''Present2
     p3 <- getFor ''Present3
     p4 <- getFor ''Present4
     return (concat [p,p1,p2,p3,p4])
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
reifyP = P . lift . reify

-- | Declare a printer for the given type name, returning an
-- expression referencing that printer.
declareP :: Name -> [TyVarBndr] -> P Exp -> P Exp
declareP name (map unkind -> tyvars) func =
  do st <- P get
     unless (any ((== (present_T name)) . fst3)
                 (pDecls st))
            (do e <- func
                P (modify (\s ->
                             s {pDecls = ((present_T name),ty,e) : pDecls s})))
     return (VarE (present_T name))
  where ty =
          ForallT (map unkind tyvars)
                  []
                  (foldr funTy
                         (presentT (foldl AppT
                                          (ConT name)
                                          (map toTy tyvars)))
                         (map (presentT . toTy) tyvars))
          where presentT t = funTy t (ConT ''Presentation)
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
               TupleT _ -> error ("Unsupported type: " ++ pprint ty ++ " (TupleT)")
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

-- | Make a constructor presenter.
makeConPresenter :: Type -> Name -> P Exp
makeConPresenter originalType thisName =
  do info <- reifyP thisName
     case info of
       TyConI dec ->
         case dec of
           DataD _ctx typeName typeVariables constructors _names ->
             do instances <- P (gets pInstances)
                case lookup typeName instances of
                  Just method ->
                    declareP typeName typeVariables (P (lift (varE method)))
                  Nothing ->
                    declareP typeName
                             typeVariables
                             (makeDataD originalType typeVariables constructors)
           TySynD _name _typeVariables _ty ->
             pure (ParensE (LamE [WildP]
                                 (AppE (ConE (mkName "Synonym"))
                                       (LitE (StringL "type synonym")))))
           x ->
             error ("Unsupported type declaration: " ++
                    pprint x ++ " (" ++ show x ++ ")")
       PrimTyConI name _arity _unlifted ->
         pure (ParensE (LamE [WildP]
                             (AppE (ConE 'Primitive)
                                   (nameE name))))
       _ -> error ("Unsupported type for presenting: " ++ show thisName)

-- | Make a printer for a data declaration.
makeDataD :: Type -> [TyVarBndr] -> [Con] -> P Exp
makeDataD originalType typeVariables constructors =
  foldl wrapInArg caseOnConstructors (reverse typeVariables)
  where wrapInArg body i =
          ParensE <$> (LamE [VarP (present_X (typeVariableName i))] <$> body)
        caseOnConstructors = LamCaseE <$> (mapM constructorCase constructors)
        constructorCase con =
          case con of
            NormalC name slots -> normalConstructor name slots
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
                   (AppE (AppE (ConE (mkName "Alg"))
                               (nameE name)) <$>
                    (ListE <$> mapM constructorSlot (zip [1 ..] slots))))
        constructorSlot (i,(_bang,typ)) =
          AppE <$> express typ <*> pure (VarE (slot_X i))
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

--------------------------------------------------------------------------------
-- Extension classes

class Present a where
  present :: a -> Presentation

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
-- Customized printers

-- Characters

instance Present Char where present = Char "Char" . return

-- Integers

instance Present Integer where present = Integer "Integer" . show
instance Present Int where present = Integer "Int" . show
instance Present Int16 where present = Integer "Int16" . show
instance Present Int32 where present = Integer "Int32" . show
instance Present Int64 where present = Integer "Int64" . show
instance Present Int8 where present = Integer "Int8" . show

-- Words

instance Present Word where present = Integer "Word" . show
instance Present Word16 where present = Integer "Word16" . show
instance Present Word32 where present = Integer "Word32" . show
instance Present Word64 where present = Integer "Word64" . show
instance Present Word8 where present = Integer "Word8" . show
