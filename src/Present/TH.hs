{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Generate a @present@ function for a type.

module Present.TH where

import Data.Data
import Data.Function
import Data.Int
import Data.Maybe
import Data.Word
import Present.Types

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift ()
import Prelude hiding (head)

-- | Convert a name to an ident.
nameToIdent :: Name -> Ident
nameToIdent (Name occ flav) =
  case flav of
    NameQ md -> Ident (PkgName "") md occ
    NameG _ pkg md -> Ident pkg md occ
    _ -> error "Unexpected name type for identifier."

-- | Make instances for all the types involved in the type.
makePresents :: Q Type -> Q [Dec]
makePresents m = m >>= generate
  where generate =
          filterNewInstances ''Present .
          fmap concat . mapM makeGenericPresent . tyNames

-- | Filter out only the new instances, removing ones that already exist.
filterNewInstances :: Name -> Q [Dec] -> Q [Dec]
filterNewInstances cls getDecs =
  do info <- reify cls
     decs <- getDecs
     return (mapMaybe (check info) decs)
  where check info dec =
          case dec of
            InstanceD _ ty _ ->
              case info of
                ClassI _ instances
                  | any (\i ->
                           case i of
                             InstanceD _ ty' _ ->
                               on (==) tyCons ty' ty
                             _ -> False)
                        instances -> Nothing
                _ -> Just dec
            _ -> Just dec

-- | Get type names.
tyNames :: Type -> [Name]
tyNames ty =
  case ty of
    ForallT _ _ x -> tyNames x
    AppT a b -> tyNames a ++ tyNames b
    SigT a _ -> tyNames a
    ConT c -> [c]
    _ -> []

-- | Get type constructors.
tyCons :: Type -> [Type]
tyCons ty =
  case ty of
    ForallT _ _ x -> tyCons x
    AppT a b -> tyCons a ++ tyCons b
    SigT a _ -> tyCons a
    VarT _ -> []
    ConT c -> [ConT c]
    PromotedT n -> [PromotedT n]
    TupleT i -> [TupleT i]
    UnboxedTupleT i -> [UnboxedTupleT i]
    ArrowT -> []
    ListT -> [ListT]
    PromotedTupleT i -> [PromotedTupleT i]
    PromotedNilT -> [PromotedNilT]
    PromotedConsT -> [PromotedConsT]
    StarT -> []
    ConstraintT -> []
    LitT lit -> [LitT lit]

-- | Generic presentation generator.
makeGenericPresent :: Name -> Q [Dec]
makeGenericPresent name =
  if elem name
          [''Char
          ,''()
          ,''[]
          ,''(,)
          ,''Integer
          ,''Int
          ,''Int8
          ,''Int16
          ,''Int32
          ,''Int64
          ,''Word
          ,''Word8
          ,''Word16
          ,''Word32
          ,''Word64
          ,''Float
          ,''Double]
     then return []
     else makeAlgPresent name

-- | Make a 'Show'-based instance of 'Present' for decimals.
makeDecimalPresent :: Name -> Q [Dec]
makeDecimalPresent name =
  filterNewInstances
    ''Present
    [d|instance Present $(conT name) where
         presentValue _mode _ _ i =
           Decimal (presentType (return i))
                   (show i)
         presentType _ = TyCon $(lift (nameToIdent name))|]

-- | Make a 'Show'-based instance of 'Present' for integrals.
makeIntegralPresent :: Name -> Q [Dec]
makeIntegralPresent name =
  filterNewInstances
    ''Present
    [d|instance Present $(conT name) where
         presentValue _mode _ _ i =
           Integral (presentType (return i))
                    (fromIntegral i)
         presentType _ = TyCon $(lift (nameToIdent name))|]

-- | Make an instance for 'Present' for the given type.
makeAlgPresent :: Name -> Q [Dec]
makeAlgPresent name =
  do info <- reify name
     filterNewInstances
       ''Present
       (case info of
          TyConI dec ->
            case dec of
              DataD _ cname tyvars cons _names ->
                fmap return
                     (makeInstance cname
                                   (vars tyvars)
                                   cons)
              NewtypeD _ cname tyvars con _names ->
                fmap return
                     (makeInstance cname
                                   (vars tyvars)
                                   [con])
              _ ->
                error ("Need a name for a type: " ++ show dec)
          _ -> error "Need a type name.")
  where vars tyvars =
          zipWith (\i _ ->
                     varT (mkName ("a" ++ show i)))
                  [1 :: Integer ..]
                  tyvars

-- | Make an instance declaration for 'Present'.
makeInstance :: Name -> [Q TH.Type] -> [Con] -> Q Dec
makeInstance name vars cons =
  instanceD ctx head [makePresentValue cons,makePresentType name vars]
  where ctx =
          sequence (map (classP ''Present .
                         return)
                        vars)
        head =
          appT (conT ''Present)
               (foldl appT (conT name) vars)

-- | Make the 'presentValue' method.
makePresentValue :: [Con] -> Q Dec
makePresentValue cons =
  let value = mkName "value"
      hierarchy = mkName "h"
      pid = mkName "pid"
      mode = mkName "mode"
  in funD 'presentValue
          [clause [varP mode,varP hierarchy,varP pid,varP value]
                  (normalB (caseE (varE value)
                                  (map (makeAlt mode hierarchy value) cons)))
                  []]

-- | Make the 'presentType' method.
makePresentType :: Name -> [TypeQ] -> DecQ
makePresentType name vars =
  funD 'presentType
       [clause (if null vars
                   then [wildP]
                   else [varP proxy])
               (normalB (foldl (\x y ->
                                  [|TyApp $(x) $(y)|])
                               [|TyCon $(lift (nameToIdent name))|]
                               (map makeTyVarRep vars)))
               []]
  where proxy = mkName "proxy"
        makeTyVarRep var =
          let needle = mkName "needle"
          in letE [sigD needle
                        (appT (appT tyFun
                                    (appT (conT ''Proxy)
                                          (foldl appT (conT name) vars)))
                              (appT (conT ''Proxy) var))
                  ,valD (varP needle)
                        (normalB (lamE [wildP]
                                       (conE 'Proxy)))
                        []]
                  (appE (varE 'presentType)
                        (appE (varE needle)
                              (varE proxy)))

-- | Because I didn't see a better way anywhere.
tyFun :: Q Type
tyFun = [t|(->)|]

-- | Make the alt for presenting a constructor.
makeAlt :: Name -> Name -> Name -> Con -> Q Match
makeAlt mode h value (RecC name slots) =
  match (conP name (map varP pvars))
        (normalB [|case $(varE (mkName "pid")) of
                     Cursor [] ->
                       Alg PrefixCons
                           (presentType (return $(varE value)))
                           (nameToIdent name)
                           $(listE (zipWith (\i var ->
                                               tupE [[|presentType (return $(varE var))|]
                                                    ,[|Cursor (return $(litE (integerL i)))|]])
                                            [0 ..]
                                            pvars))
                     Cursor (i:j) ->
                       $(if null pvars
                            then [|error ("Unexpected case for Present: " ++
                                          $(litE (stringL (show name))) ++
                                          "Index: " ++
                                          show (i,j))|]
                            else caseE [|i|]
                                       (zipWith (\ij var ->
                                                   (match (if ij ==
                                                              fromIntegral (length pvars) -
                                                              1
                                                              then wildP
                                                              else litP (integerL ij))
                                                          (normalB [|presentValue $(varE mode)
                                                                                  $(varE h)
                                                                                  (Cursor j)
                                                                                  $(varE var)|])
                                                          []))
                                                [0 ..]
                                                pvars))|])
        []
  where pvars =
          zipWith makeSlot [1 :: Integer ..] slots
        makeSlot x _ = mkName ("slot" ++ show x)
makeAlt mode h value (NormalC name slots) =
  match (conP name (map varP pvars))
        (normalB [|case $(varE (mkName "pid")) of
                     Cursor [] ->
                       Alg PrefixCons
                           (presentType (return $(varE value)))
                           (nameToIdent name)
                           $(listE (zipWith (\i var ->
                                               tupE [[|presentType (return $(varE var))|]
                                                    ,[|Cursor (return $(litE (integerL i)))|]])
                                            [0 ..]
                                            pvars))
                     Cursor (i:j) ->
                       $(if null pvars
                            then [|error ("Unexpected case for Present: " ++
                                          $(litE (stringL (show name))) ++
                                          "Index: " ++
                                          show (i,j))|]
                            else caseE [|i|]
                                       (zipWith (\ij var ->
                                                   (match (if ij ==
                                                              fromIntegral (length pvars) -
                                                              1
                                                              then wildP
                                                              else litP (integerL ij))
                                                          (normalB [|presentValue $(varE mode)
                                                                                  $(varE h)
                                                                                  (Cursor j)
                                                                                  $(varE var)|])
                                                          []))
                                                [0 ..]
                                                pvars))|])
        []
  where pvars =
          zipWith makeSlot [1 :: Integer ..] slots
        makeSlot x _ = mkName ("slot" ++ show x)
makeAlt mode h value (InfixC slot1 name slot2) =
  makeAlt mode h value (NormalC name [slot1,slot2])
makeAlt _ _ _ c = error ("makePresent.makeAlt: Unexpected case: " ++ show c)
