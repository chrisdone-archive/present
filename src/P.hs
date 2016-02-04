module P where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List

data Presentation = Cons Name [Presentation]
  deriving (Show)

present :: Name -> Q Exp
present name =
  do VarI _ ty _ _ <- reify name
     appE (makePresenter ty)
          (varE name)

makePresenter :: Type -> Q Exp
makePresenter ty =
   case ty of
     ForallT _ _ _ -> error ("Unsupported type: " ++ show "ForallT")
     AppT f a -> appE (makePresenter f)
                      (makePresenter a)
     SigT _ _ -> error ("Unsupported type: " ++ show "SigT")
     VarT _ -> error ("Unsupported type: " ++ show "VarT")
     ConT name -> makeConPresenter name
     PromotedT _ -> error ("Unsupported type: " ++ show "PromotedT")
     TupleT _ -> error ("Unsupported type: " ++ show "TupleT")
     UnboxedTupleT _ -> error ("Unsupported type: " ++ show "UnboxedTupleT")
     ArrowT -> error ("Unsupported type: " ++ show "ArrowT")
     EqualityT -> error ("Unsupported type: " ++ show "EqualityT")
     ListT -> error ("Unsupported type: " ++ show "ListT")
     PromotedTupleT _ -> error ("Unsupported type: " ++ show "PromotedTupleT")
     PromotedNilT -> error ("Unsupported type: " ++ show "PromotedNilT")
     PromotedConsT -> error ("Unsupported type: " ++ show "PromotedConsT")
     StarT -> error ("Unsupported type: " ++ show "StarT")
     ConstraintT -> error ("Unsupported type: " ++ show "ConstraintT")
     LitT _ -> error ("Unsupported type: " ++ show "LitT")

makeConPresenter :: Name -> Q Exp
makeConPresenter name =
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

                                                         appE (makePresenter ty)
                                                              (varE (var i)))
                                                  (zip [1..] slots))))
                                       []
                                _ ->
                                  case con of
                                    NormalC _ _ -> error ("NormalC")
                                    RecC _ _ -> error ("RecC")
                                    InfixC _ _ _ -> error ("InfixC")
                                    ForallC _ _ _ -> error ("ForallC"))
                           cons))))
               (reverse (zipWith const [1..] tyvars))
             where a = mkName "a"
                   tyVarMatch name ty =
                     case ty of
                       PlainTV name' -> name == name'
                       KindedTV name' _ -> name == name'
       PrimTyConI name _arity _unlifted ->
         parensE (lamE [wildP] (stringE ("Primitive: " ++ show name)))
       _ -> error ("Unsupported type for presenting: " ++ show name)
  where tyvar i = mkName ("tyvar" ++ show i)
