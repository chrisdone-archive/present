-- TODO: handle recursive types
-- TODO: type aliases

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module P where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List

data Presentation
  = Primitive String
  | Alg String
        [Presentation]
  | Rec String [(String,Presentation)]
  deriving (Show)

data Record =
  Record {foo :: Maybe Int
         ,bar :: Char}

example = Record {foo=Just 123,bar='a'}

-- | Present the given name.
present :: Name -> Q Exp
present name =
  do result <- try (reify name)
     case result of
       Nothing -> fail ("The name \"" ++ show name ++ "\" isn't in scope.")
       Just (VarI _ ty _ _) -> appE (makePresenter ty ty)
          (varE name)
  where try m = recover (pure Nothing) (fmap Just m)

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
         appE (makePresenter ty ty)
              (varE name)
  where try m =
          recover (pure Nothing)
                  (fmap Just m)
        name = mkName "it"

help lines =
  fail (unlines (take 1 lines ++ map ("    " ++) (drop 1 (lines ++ feedback))))
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

makePresenter :: Type -> Type -> Q Exp
makePresenter originalType ty =
  case ty of
    AppT f a ->
      appE (makePresenter originalType f)
           (makePresenter originalType a)
    ConT name -> makeConPresenter originalType name
    ForallT _vars _ctx ty ->
      makePresenter originalType ty
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

makeConPresenter :: Type -> Name -> Q Exp
makeConPresenter originalType name =
  do info <- reify name
     case info of
       TyConI dec ->
         case dec of
           DataD _ctx name tyvars cons _names ->
             foldl
               (\acc i -> parensE (lamE [varP (tyvar i)] acc))
               (parensE
                  (lamE
                     [varP a]
                     (caseE
                        (varE a)
                        (map
                           (\con ->
                              case con of
                                NormalC name slots ->
                                  let var = mkName . ("p"++) . show
                                  in match
                                       (conP name
                                             (map (varP . var . fst)
                                                  (zip [1..] slots)))
                                       (normalB
                                          (appE (appE
                                                   (conE (mkName "Alg"))
                                                   (nameE name))
                                                (listE
                                                   (map
                                                        (\(i,(_strictness,ty)) ->
                                                           case ty of
                                                             VarT vname ->
                                                               case find (tyVarMatch vname . fst)
                                                                         (zip tyvars [1..]) of
                                                                 Nothing -> error "Invalid type variable in constructor."
                                                                 Just (_,x) ->
                                                                   appE (varE (tyvar x))
                                                                        (varE (var i))
                                                             ty ->
                                                               appE (makePresenter originalType ty)
                                                                    (varE (var i)))
                                                        (zip [1..] slots)))))
                                       []
                                RecC name (fields :: [(Name,Strict,Type)]) ->
                                  let var = mkName . ("p"++) . show
                                  in match
                                       (conP name
                                             (map (varP . var . fst)
                                                  (zip [1..] fields)))
                                       (normalB
                                          (appE (appE
                                                   (conE (mkName "Rec"))
                                                   (nameE name))
                                                (listE
                                                   (map
                                                        (\(i,(name,_strictness,ty)) ->
                                                           tupE
                                                                [nameE name
                                                                ,case ty of
                                                                   VarT vname ->
                                                                     case find (tyVarMatch vname . fst)
                                                                               (zip tyvars [1..]) of
                                                                       Nothing -> error "Invalid type variable in constructor."
                                                                       Just (_,x) ->
                                                                         appE (varE (tyvar x))
                                                                              (varE (var i))
                                                                   ty ->
                                                                     appE (makePresenter originalType ty)
                                                                          (varE (var i))])
                                                        (zip [1..] fields)))))
                                       []
                                _ ->
                                  case con of
                                    NormalC _ _ -> error ("NormalC")
                                    RecC _ _ -> error ("RecC")
                                    InfixC _ _ _ -> error ("InfixC")
                                    ForallC _ _ _ -> error ("ForallC"))
                           cons))))
               (reverse (zipWith const [1..] tyvars))
           NewtypeD _ctx name tyvars con _names ->
             let cons = [con]
             in foldl
                  (\acc i -> parensE (lamE [varP (tyvar i)] acc))
                  (parensE
                     (lamE
                        [varP a]
                        (caseE
                           (varE a)
                           (map
                              (\con ->
                                 case con of
                                   NormalC name slots ->
                                     let var = mkName . ("p"++) . show
                                     in match
                                          (conP name
                                                (map (varP . var . fst)
                                                     (zip [1..] slots)))
                                          (normalB
                                             (appE (appE
                                                      (conE (mkName "Alg"))
                                                      (nameE name))
                                                   (listE
                                                      (map
                                                           (\(i,(_strictness,ty)) ->
                                                              case ty of
                                                                VarT vname ->
                                                                  case find (tyVarMatch vname . fst)
                                                                            (zip tyvars [1..]) of
                                                                    Nothing -> error "Invalid type variable in constructor."
                                                                    Just (_,x) ->
                                                                      appE (varE (tyvar x))
                                                                           (varE (var i))
                                                                ty ->
                                                                  appE (makePresenter originalType ty)
                                                                       (varE (var i)))
                                                           (zip [1..] slots)))))
                                          []
                                   RecC name (fields :: [(Name,Strict,Type)]) ->
                                     let var = mkName . ("p"++) . show
                                     in match
                                          (conP name
                                                (map (varP . var . fst)
                                                     (zip [1..] fields)))
                                          (normalB
                                             (appE (appE
                                                      (conE (mkName "Rec"))
                                                      (nameE name))
                                                   (listE
                                                      (map
                                                           (\(i,(name,_strictness,ty)) ->
                                                              tupE
                                                                   [nameE name
                                                                   ,case ty of
                                                                      VarT vname ->
                                                                        case find (tyVarMatch vname . fst)
                                                                                  (zip tyvars [1..]) of
                                                                          Nothing -> error "Invalid type variable in constructor."
                                                                          Just (_,x) ->
                                                                            appE (varE (tyvar x))
                                                                                 (varE (var i))
                                                                      ty ->
                                                                        appE (makePresenter originalType ty)
                                                                             (varE (var i))])
                                                           (zip [1..] fields)))))
                                          []
                                   _ ->
                                     case con of
                                       NormalC _ _ -> error ("NormalC")
                                       RecC _ _ -> error ("RecC")
                                       InfixC _ _ _ -> error ("InfixC")
                                       ForallC _ _ _ -> error ("ForallC"))
                              cons))))
                  (reverse (zipWith const [1..] tyvars))
           TySynD name tyvars ty ->
             parensE (lamE [wildP]
                           (appE
                              (conE (mkName "Primitive"))
                              (stringE "type synonym")))
           x -> error ("Unsupported type declaration: " ++ pprint x ++ " (" ++ show x ++ ")")
       PrimTyConI name _arity _unlifted ->
         parensE
           (lamE
              [wildP]
              (appE
                 (conE (mkName "Primitive"))
                 (nameE name)))
       _ -> error ("Unsupported type for presenting: " ++ show name)
  where a = mkName "a"
        tyvar i = mkName ("tyvar" ++ show i)
        nameE = stringE . show
        tyVarMatch name ty =
          case ty of
            PlainTV name' -> name == name'
            KindedTV name' _ -> name == name'
