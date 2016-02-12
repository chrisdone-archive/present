{-# LANGUAGE LambdaCase #-}
-- Remaining work:

-- TODO: cleanup
--
-- TODO: need to add type-signatures to retain let generalization
--
-- TODO: type aliases
--
-- TODO: consider defaulting common classes like Num, IsString, etc.
--
-- TODO: consider supporting printing e.g. `Proxy :: Proxy a` or `Left
-- 'a' :: Either Char a`. In other words, if it's unambiguous (none of
-- the data slots depend on a type variable), might as well print
-- it. GHCi supports this by defaulting, we can support it by just
-- ignoring unused type variables.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Generate presentations for a data type.

module Present
  (presentIt
  ,present
  ,Presentation(..)
  ,Record(..)
  ,example
  )
  where

import Control.Monad.State.Strict
-- -- import Data.List

import Language.Haskell.TH

data Record =
  Record {foo :: Maybe Int
         ,bar :: Char}

example :: Record
example = Record {foo=Just 123,bar='a'}

-- | A presentation of a data structure.
data Presentation
  = Primitive String
  | Alg String
        [Presentation]
  | Rec String [(String,Presentation)]
  deriving (Show)

--------------------------------------------------------------------------------
-- Printing monad

-- | The presentation generating monad. Transforms over Q and spits
-- out declarations.
newtype P a = P { unP :: StateT PState Q a}
  deriving (Applicative,Monad,Functor)

data PState =
  PState {pDecls :: [(Name,Exp)]
         ,pTypes :: [(Type,Exp)]
         ,pTypesCache :: [(Type,Exp)]
         ,pMakeDecls :: Bool}

-- | Reify a name.
reifyP :: Name -> P Info
reifyP = P . lift . reify

-- | Declare a printer for the given type name, returning an
-- expression referencing that printer.
declareP :: Name -> P Exp -> P Exp
declareP name func =
  do st <- P get
     unless (any ((==(funcName name)) . fst) (pDecls st))
            (do e <- func
                P (modify (\s -> s {pDecls = ((funcName name),e) : pDecls s})))
     return (VarE (funcName name))

-- | Given the name of a type Foo, make a function name like p_Foo.
funcName :: Name -> Name
funcName name = mkName ("present_" ++ concatMap normalize (show name))
  where normalize c =
          case c of
            '_' -> "__"
            '.' -> "_"
            _ -> [c]

-- | Make a declaration given the name and type.
makeDec :: Name -> Exp -> Q Dec
makeDec name e = return (ValD (VarP name) (NormalB e) [])

--------------------------------------------------------------------------------
-- Top-level functions

-- | Present the given name.
present :: Name -> Q Exp
present name =
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
  do (func,PState decls _ _ _) <-
       runStateT (unP (do e <- makePresenter ty ty
                          -- We do a first run without decl generation enabled,
                          -- which avoids recursive types.
                          -- Then we enable decl generation and re-generate.
                          P (modify (\s -> s {pMakeDecls = True, pTypes = [], pTypesCache = pTypes s}))
                          _ <- makePresenter ty ty
                          return e))
                 (PState [] [] [] False)
     letE (map (uncurry makeDec) decls)
          (appE (return func)
                (varE name))

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
                                  else return (VarE (funcName name))
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

-- | Make a constructor presenter.
makeConPresenter :: Type -> Name -> P Exp
makeConPresenter originalType thisName =
  do info <- reifyP thisName
     case info of
       TyConI dec ->
         case dec of
           DataD _ctx typeName typeVariables constructors _names ->
             declareP typeName
                      (makeDataD originalType typeVariables constructors)
           TySynD _name _typeVariables _ty ->
             pure (ParensE (LamE [WildP]
                                 (AppE (ConE (mkName "Primitive"))
                                       (LitE (StringL "type synonym")))))
           x ->
             error ("Unsupported type declaration: " ++
                    pprint x ++ " (" ++ show x ++ ")")
       PrimTyConI name _arity _unlifted ->
         pure (ParensE (LamE [WildP]
                             (AppE (ConE (mkName "Primitive"))
                                   (nameE name))))
       _ -> error ("Unsupported type for presenting: " ++ show thisName)

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

slot_X :: Int -> Name
slot_X = mkName . ("slot_" ++) . show

present_X :: Name -> Name
present_X i = mkName ("present_" ++ show i)

typeVariableName :: TyVarBndr -> Name
typeVariableName (PlainTV name) = name
typeVariableName (KindedTV name _) = name

nameE :: Name -> Exp
nameE = LitE . StringL . show

stringP :: String -> Q Exp
stringP = return . LitE . StringL

tyVarMatch :: Name -> TyVarBndr -> Bool
tyVarMatch name ty =
  case ty of
    PlainTV name' -> name == name'
    KindedTV name' _ -> name == name'
