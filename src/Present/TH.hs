{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Generate a @present@ function for a type.

module Present.TH where

import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Haskell.TH as TH
import           Prelude hiding (head)
import           Present.Types

-- | Make an instance for 'Present' for the given type.
makePresent :: Name -> Q [Dec]
makePresent name =
  do info <- reify name
     case info of
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
       _ -> error "Need a type name."
  where vars tyvars =
          zipWith (\i _ ->
                     varT (mkName ("a" ++ show i)))
                  [1 :: Integer ..]
                  tyvars

-- | Make an instance declaration for 'Present-.
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
  in funD 'presentValue
          [clause [varP hierarchy,varP pid,varP value]
                  (normalB (caseE (varE value)
                                  (map (makeAlt hierarchy value) cons)))
                  []]

-- | Make the 'presentType' method.
makePresentType :: Name -> [TypeQ] -> DecQ
makePresentType name vars =
  funD 'presentType
       [clause (if null vars
                   then [wildP]
                   else [varP proxy])
               (normalB (appE (conE 'Type)
                              (appE (varE 'T.unwords)
                                    (appE [|map typeText|]
                                          (listE (litE (stringL (nameBase name)) :
                                                  map makeTyVarRep vars))))))
               []]
  where proxy = mkName "proxy"
        makeTyVarRep var =
          let needle = mkName "needle"
          in letE [sigD needle
                        (appT (appT arrowT
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

-- | Make the alt for presenting a constructor.
makeAlt :: Name -> Name -> Con -> Q Match
makeAlt h value (RecC name slots) =
  match (conP name (map varP pvars))
        (normalB [|case $(varE (mkName "pid")) of
                     Cursor [] ->
                       Alg (presentType (return $(varE value)))
                           $(litE (stringL (nameBase name)))
                           $(listE (zipWith (\i var ->
                                               tupE [[|presentType (return $(varE var))|]
                                                    ,[|Cursor (return $(litE (integerL i)))|]])
                                            [0 ..]
                                            pvars))
                     Cursor (i:j) ->
                       $(if null pvars
                            then [|error ("Unexpected case for Present: " ++
                                          $(litE (stringL (show name))) ++
                                          "Index: " ++ show (i,j))|]
                            else caseE [|i|]
                                       (zipWith (\ij var ->
                                                   (match (if ij == fromIntegral (length pvars) - 1
                                                              then wildP
                                                              else litP (integerL ij))
                                                          (normalB [|presentValue $(varE h)
                                                                                  (Cursor j)
                                                                                  $(varE var)|])
                                                          []))
                                                [0 ..]
                                                pvars))|])
        []
  where pvars = zipWith makeSlot [1 :: Integer ..] slots
        makeSlot x _ = mkName ("x" ++ show x)
makeAlt h value (NormalC name slots) =
  match (conP name (map varP pvars))
        (normalB [|case $(varE (mkName "pid")) of
                     Cursor [] ->
                       Alg (presentType (return $(varE value)))
                           $(litE (stringL (nameBase name)))
                           $(listE (zipWith (\i var ->
                                               tupE [[|presentType (return $(varE var))|]
                                                    ,[|Cursor (return $(litE (integerL i)))|]])
                                            [0 ..]
                                            pvars))
                     Cursor (i:j) ->
                       $(if null pvars
                            then [|error ("Unexpected case for Present: " ++
                                          $(litE (stringL (show name))) ++
                                          "Index: " ++ show (i,j))|]
                            else caseE [|i|]
                                       (zipWith (\ij var ->
                                                   (match (if ij == fromIntegral (length pvars) - 1
                                                              then wildP
                                                              else litP (integerL ij))
                                                          (normalB [|presentValue $(varE h)
                                                                                  (Cursor j)
                                                                                  $(varE var)|])
                                                          []))
                                                [0 ..]
                                                pvars))|])
        []
  where pvars = zipWith makeSlot [1 :: Integer ..] slots
        makeSlot x _ = mkName ("x" ++ show x)
makeAlt h value (InfixC slot1 name slot2) =
  makeAlt h value (NormalC name [slot1,slot2])
makeAlt _ _ c = error ("makePresent.makeAlt: Unexpected case: " ++ show c)
