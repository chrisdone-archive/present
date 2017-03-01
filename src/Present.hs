{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-- | Generate presentations for types.

module Present
  (-- * Presenting functions
   presentIt
  ,presentName
  ,presentType
  -- * Presentation mediums
  ,toShow
  ,toWHNF
  ,whnfJson
  -- * Debugging convenience functions
  ,presentShow
  -- * Types
  ,Value(..)
  ,WHNF(..)
  -- * Customization classes
  ,Present0(..)
  ,Present1(..)
  ,Present2(..)
  ,Present3(..)
  ,Present4(..)
  ,Present5(..)
  ,Present6(..))
  where

import           Control.Arrow (second)
import           Control.Exception (evaluate,SomeException(..),try,evaluate)
import           Control.Monad.Trans.State.Strict (evalStateT,get,modify,StateT(..))
import           Data.Char (isSpace,ord,isAlphaNum)
import           Data.Int (Int8,Int16,Int32,Int64)
import           Data.List (nub,find,intercalate,foldl',isSuffixOf)
import           Data.Maybe (mapMaybe,isJust)
import           Data.Ratio (numerator,denominator)
import           Data.String (IsString)
import           Data.Typeable (typeOf)
import           Data.Word (Word8,Word32,Word64)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Numeric (showHex)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Printf (printf)

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

--------------------------------------------------------------------------------
-- Introduction
--
-- Present's algorithm works in stages/levels of work. The first
-- implementation of Present worked, but was a mess. This
-- implementation is an effort to separate all that functionality into
-- clean, small stages.

--------------------------------------------------------------------------------
-- Type Normalization
--
-- TH's representation of types is wider than what Present cares
-- about, and has some heterogeneity that is unwanted; it adds some
-- messiness. To simplify the code further down, we perform this
-- transformation to simplify the types and therefore the scope we
-- have to deal with.
--
-- TH's representation of names is also unfortunate, as it mixes up
-- variables, types and type variables all together. We address that
-- too.

-- | A type variable.
newtype TypeVariable =
  TypeVariable TH.Name
  deriving (Eq)

-- | A type constructor.
newtype TypeConstructor =
  TypeConstructor TH.Name
  deriving (Eq)

-- | A primitive type constructor.
newtype PrimitiveTypeConstructor =
  PrimitiveTypeConstructor TH.Name

-- | A normalized type.
data NormalType
  = NormalCons TypeConstructor
  | NormalPrimitive PrimitiveTypeConstructor
  | NormalFunction TH.Type
  | NormalVar TypeVariable
  | NormalApp NormalType
              [NormalType]


-- | Convert the heterogenous TH type into a more normal form.
normalizeType
  :: TH.Type -> Either String NormalType
normalizeType = go
  where go =
          \case
            ty@TH.AppT{} ->
              do let (typeFunction,typeArguments) = flattenApplication ty
                 case typeFunction of
                   TH.ArrowT -> return (NormalFunction ty)
                   _ -> NormalApp <$> go typeFunction <*> mapM go typeArguments
            TH.ForallT _ context ty ->
              if isFunction ty
                 then return (NormalFunction ty)
                 else go (typeClassDefaulting context ty)
            TH.SigT ty _kind -> go ty
            TH.VarT name -> return (NormalVar (TypeVariable name))
            TH.ConT name ->
              return (if isPrimitiveType name
                         then NormalPrimitive (PrimitiveTypeConstructor name)
                         else NormalCons (TypeConstructor name))
            TH.TupleT i ->
              case lookup i tupleConstructors of
                Nothing -> fail ("Tuple arity " ++ show i ++ " not supported.")
                Just cons -> return (NormalCons (TypeConstructor cons))
            TH.ListT -> return (NormalCons (TypeConstructor ''[]))
            TH.PromotedT _ -> fail "Promoted types are not supported."
            TH.UnboxedTupleT _ -> fail "Unboxed tuples are not supported."
            TH.ArrowT -> fail "The function arrow (->) is not supported."
            TH.EqualityT -> fail "Equalities are not supported."
            TH.PromotedTupleT _ -> fail "Promoted types are not supported."
            TH.PromotedNilT -> fail "Promoted types are not supported."
            TH.PromotedConsT -> fail "Promoted types are not supported."
            TH.StarT -> fail "Star (*) is not supported."
            TH.ConstraintT -> fail "Constraints are not supported."
            TH.LitT _ -> fail "Type-level literals are not supported."
            TH.InfixT{} -> fail "Infix type constructors are not supported."
            TH.UInfixT{} -> fail "Unresolved infix type constructors are not supported."
            TH.ParensT _ -> fail "Parenthesized types are not supported."
            TH.WildCardT -> fail "Wildcard types are not supported."

-- | Is the type a function?
isFunction :: TH.Type -> Bool
isFunction ty =
  let (typeFunction,_) = flattenApplication ty
  in case typeFunction of
       TH.ArrowT -> True
       _ -> False

-- | Arity-constructor mapping for tuples.
tupleConstructors :: [(Int,TH.Name)]
tupleConstructors =
  [(0,''())
  ,(2,''(,))
  ,(3,''(,,))
  ,(4,''(,,,))
  ,(5,''(,,,,))
  ,(6,''(,,,,,))
  ,(7,''(,,,,,,))
  ,(8,''(,,,,,,,))
  ,(9,''(,,,,,,,,))
  ,(10,''(,,,,,,,,,))
  ,(11,''(,,,,,,,,,,))
  ,(12,''(,,,,,,,,,,,))
  ,(13,''(,,,,,,,,,,,,))
  ,(14,''(,,,,,,,,,,,,,))
  ,(15,''(,,,,,,,,,,,,,,))
  ,(16,''(,,,,,,,,,,,,,,,))
  ,(17,''(,,,,,,,,,,,,,,,,))
  ,(18,''(,,,,,,,,,,,,,,,,,))
  ,(19,''(,,,,,,,,,,,,,,,,,,))
  ,(20,''(,,,,,,,,,,,,,,,,,,,))
  ,(21,''(,,,,,,,,,,,,,,,,,,,,))
  ,(22,''(,,,,,,,,,,,,,,,,,,,,,))
  ,(23,''(,,,,,,,,,,,,,,,,,,,,,,))
  ,(24,''(,,,,,,,,,,,,,,,,,,,,,,,))
  ,(25,''(,,,,,,,,,,,,,,,,,,,,,,,,))
  ,(26,''(,,,,,,,,,,,,,,,,,,,,,,,,,))]

-- | Is the name specified by Name a primitive type? Like Int#?
--
-- This check may be overly cautious, but it's also about as accurate
-- as one can seemingly be.
isPrimitiveType :: TH.Name -> Bool
isPrimitiveType (TH.Name (TH.OccName _) (TH.NameG TH.TcClsName (TH.PkgName "ghc-prim") (TH.ModName "GHC.Prim"))) =
  True
isPrimitiveType name = isSuffixOf "#" (show name)

-- | Flatten a type application f x y into (f,[x,y]).
flattenApplication
  :: TH.Type -> (TH.Type,[TH.Type])
flattenApplication = go []
  where go args (TH.AppT f x) = go (x : args) f
        go args f = (f,args)

--------------------------------------------------------------------------------
-- Defaulting
--
-- For some classes like Num and IsString, we can default to a
-- reasonable value in the REPL. It leads to a better user-experience.

-- | Apply defaulted substitutions for each of the constraints in the
-- type.
typeClassDefaulting
  :: [TH.Type] -> TH.Type -> TH.Type
typeClassDefaulting constraints =
  applyTypeSubstitution
    (mapMaybe (\case
                 TH.AppT (TH.ConT className) (TH.VarT varName) ->
                   fmap (\tyName -> (varName,TH.ConT tyName))
                        (lookup className defaultedClasses)
                 _ -> Nothing)
              constraints)

-- | Apply the given substitutions to the type.
applyTypeSubstitution
  :: [(TH.Name,TH.Type)] -> TH.Type -> TH.Type
applyTypeSubstitution subs = go
  where go =
          \case
            TH.ForallT vars ctx ty ->
              TH.ForallT vars
                         ctx
                         (go ty)
            TH.AppT f x ->
              TH.AppT (go f)
                      (go x)
            TH.SigT ty k -> TH.SigT (go ty) k
            TH.VarT a
              | Just b <- lookup a subs -> b
              | otherwise -> TH.VarT a
            x -> x

-- | Classes which when encountered in a forall context should have
-- their corresponding type variables substituted on the right hand
-- side with the given type.
defaultedClasses :: [(TH.Name,TH.Name)]
defaultedClasses =
  [(''Integral,''Integer)
  ,(''Num,''Integer)
  ,(''Fractional,''Double)
  ,(''Bounded,''())
  ,(''Eq,''())
  ,(''Read,''())
  ,(''Show,''())
  ,(''IsString,''String)]

--------------------------------------------------------------------------------
-- Type Enumeration
--
-- Given a NormalType, we extract all the instances of NormalCons into
-- a flat set.
--
-- We can then run through each type constructor name, reify them, and
-- generate a printer for it. This separate step avoids cycles/acts as
-- an alternative to performing an occurs check.

-- | Enumerate all unique type constructors in the type.
enumerateTypeConstructors
  :: NormalType -> [TypeConstructor]
enumerateTypeConstructors = nub . go
  where go =
          \case
            NormalCons cons -> [cons]
            NormalApp ty tys -> go ty ++ concatMap go tys
            NormalPrimitive{} -> []
            NormalVar{} -> []
            NormalFunction{} -> []

--------------------------------------------------------------------------------
-- Type Reification
--
-- We have to reify all the type constructors involved in a given
-- type.
--

-- | Name of a variable.
newtype ValueVariable =
  ValueVariable TH.Name

-- | Name of a value constructor.
newtype ValueConstructor =
  ValueConstructor TH.Name

-- | A normalize representation of a constructor. Present's main
-- algorithm doesn't particularly care whether it's infix, a record,
-- or whatever.
data Constructor =
  Constructor {_constructorName :: ValueConstructor
              ,constructorFields :: [(Maybe ValueVariable,NormalType)]}

-- | A data type.
data DataType =
  DataType {_dataTypeVariables :: [TypeVariable]
           ,_dataTypeConstructors :: [Constructor]}

-- | A type alias.
data TypeAlias =
  TypeAlias {_aliasVariables :: [TypeVariable]
            ,_aliasType :: NormalType}

-- | Definition of a type.
data TypeDefinition
  = DataTypeDefinition TypeConstructor
                       DataType
  | TypeAliasDefinition TypeConstructor
                        TypeAlias

-- | Reify all the constructors of a name. Unless it's primitive, in
-- which case return nothing.
reifyTypeDefinition
  :: TypeConstructor -> TH.Q (Maybe TypeDefinition)
reifyTypeDefinition typeConstructor@(TypeConstructor name) =
  do info <- TH.reify name
     let result =
           case info of
             TH.TyConI dec ->
               case dec of
                 TH.DataD _cxt0 _ vars _mkind cons _cxt1 ->
                   do cs <- mapM makeConstructor cons
                      return (Just (DataTypeDefinition typeConstructor
                                                       (DataType (map toTypeVariable vars) cs)))
                 TH.NewtypeD _cxt0 _ vars _mkind con _cxt1 ->
                   do c <- makeConstructor con
                      return (Just (DataTypeDefinition
                                      typeConstructor
                                      (DataType (map toTypeVariable vars)
                                                [c])))
                 TH.TySynD _ vars ty ->
                   do ty' <- normalizeType ty
                      return (Just (TypeAliasDefinition typeConstructor
                                                        (TypeAlias (map toTypeVariable vars) ty')))
                 _ -> fail "Not a supported data type declaration."
             TH.PrimTyConI{} -> return Nothing
             TH.FamilyI{} -> fail "Data families not supported yet."
             _ ->
               fail ("Not a supported object, no type inside it: " ++
                     TH.pprint info)
     case result of
       Left err -> fail err
       Right ok -> return ok

-- | Convert a TH type variable to a normalized type variable.
toTypeVariable :: TH.TyVarBndr -> TypeVariable
toTypeVariable =
  \case
    TH.PlainTV t -> TypeVariable t
    TH.KindedTV t _ -> TypeVariable t

-- | Make a normalized constructor from the more complex TH Con.
makeConstructor
  :: TH.Con -> Either String Constructor
makeConstructor =
  \case
    TH.NormalC name slots ->
      Constructor <$> pure (ValueConstructor name) <*> mapM makeSlot slots
    TH.RecC name fields ->
      Constructor <$> pure (ValueConstructor name) <*> mapM makeField fields
    TH.InfixC t1 name t2 ->
      Constructor <$> pure (ValueConstructor name) <*>
      ((\x y -> [x,y]) <$> makeSlot t1 <*> makeSlot t2)
    (TH.ForallC _ _ con) ->
      makeConstructor con
    TH.GadtC _ _ _ ->
      undefined -- FIXME
    TH.RecGadtC _ _ _ ->
      undefined -- FIXME
  where makeSlot (_,ty) = (Nothing,) <$> normalizeType ty
        makeField (name,_,ty) =
          (Just (ValueVariable name),) <$> normalizeType ty

--------------------------------------------------------------------------------
-- Definition Elaboration
--
-- When reifying a type, we discover that it refers to other types
-- which in turn need to be reified. So to get the total of all types
-- that we're going to want to generate a printer for, we need to
-- recursively elaborate everything all the way down.
--
-- A primitive type definition does not decompose into other types.

-- | Elaborate the types involved in a type definition.
definitionNormalTypes
  :: TypeDefinition -> [NormalType]
definitionNormalTypes =
  \case
    DataTypeDefinition _ (DataType _ cons) ->
      concatMap (map snd . constructorFields) cons
    TypeAliasDefinition _ (TypeAlias _ ty) -> [ty]

--------------------------------------------------------------------------------
-- Complete Expansion
--
-- Finally, we need a way to, given a type, completely explode that
-- type, and every type inside it, recursively, to produce a finite,
-- unique set of TypeDefinitions.

-- | Expand a type into all the type definitions directly or
-- indirectly related.
normalTypeDefinitions
  :: NormalType -> TH.Q [TypeDefinition]
normalTypeDefinitions = flip evalStateT [] . expandNormalType
  where expandNormalType =
          fmap concat . mapM expandTypeConstructor . enumerateTypeConstructors
        expandTypeConstructor typeConstructor =
          do seenConstructors <- get
             if elem typeConstructor seenConstructors
                then return []
                else do mtypeDefinition <-
                          liftQ (reifyTypeDefinition typeConstructor)
                        case mtypeDefinition of
                          Nothing -> return []
                          Just typeDefinition ->
                            do let normalTypes =
                                     definitionNormalTypes typeDefinition
                               modify (typeConstructor :)
                               typeDefinitions <-
                                 fmap concat (mapM expandNormalType normalTypes)
                               return (typeDefinition : typeDefinitions)

-- | Lift a Q monad into a StateT transformer.
liftQ :: TH.Q a -> StateT s TH.Q a
liftQ m =
  StateT (\s ->
            do v <- m
               return (v,s))

--------------------------------------------------------------------------------
-- Printer Generation
--
-- Given a TypeDefinition, generate a printer for that data type.

data Value
  = DataValue String String [Value]
  | TypeVariableValue String
  | PrimitiveValue String
  | FunctionValue String
  | CharValue String String
  | IntegerValue String String
  | ChoiceValue String [(String,Value)]
  | RecordValue String String [(String,Value)]
  | ListValue String [Value]
  | StringValue String String
  | TupleValue String [Value]
  | ExceptionValue String String
  deriving (Show)

-- | Make a presenter for a type definition.
typeDefinitionPresenter :: [(TypeConstructor,ValueVariable)]
                        -> TypeDefinition
                        -> TH.Q [TH.Dec]
typeDefinitionPresenter instances =
  \case
    DataTypeDefinition typeConstructor dataType@(DataType typeVariables _) ->
      case find (namesBasicallyEqual typeConstructor . fst) instances of
        Nothing ->
          case find (namesBasicallyEqual typeConstructor . fst) builtInPresenters of
            Nothing -> dataTypePresenter typeConstructor dataType
            Just (_,presenter) ->
              do automaticPresenter <-
                   dataTypePresenterBody typeConstructor dataType
                 builtinFunctionDeclaration typeConstructor
                                            (presenter typeVariables automaticPresenter)
        Just (_,methodName) ->
          do instanceBasedPresenter typeConstructor methodName dataType typeVariables
    TypeAliasDefinition typeConstructor typeAlias ->
      typeAliasPresenter typeConstructor typeAlias

-- | Make a printer based on an instance declaration for Present[N].
instanceBasedPresenter :: TypeConstructor
                       -> ValueVariable
                       -> DataType
                       -> [TypeVariable]
                       -> TH.Q [TH.Dec]
instanceBasedPresenter typeConstructor@(TypeConstructor typeConstructorName) (ValueVariable methodName) dataType typeVariables =
  presentingFunctionDeclaration
    typeConstructor
    typeVariables
    (TH.tupE [typeDisplayExpression
             ,[|\x ->
                  ChoiceValue
                    $(typeDisplayExpression)
                    [("Instance"
                     ,snd $(foldl TH.appE
                                  (TH.varE methodName)
                                  (map (TH.varE . presentVarName) typeVariables))
                          x)
                    ,("Internal"
                     ,$(dataTypePresenterBody typeConstructor dataType) x)]|]])
  where typeDisplayExpression = typeDisplay typeVariables typeConstructorName

-- | Make a presenter for the given data type.
dataTypePresenter
  :: TypeConstructor -> DataType -> TH.Q [TH.Dec]
dataTypePresenter typeConstructor@(TypeConstructor typeConstructorName) dataType@(DataType typeVariables _) =
  presentingFunctionDeclaration
    typeConstructor
    typeVariables
    (TH.tupE [typeDisplayExpression
             ,dataTypePresenterBody typeConstructor dataType])
  where typeDisplayExpression = typeDisplay typeVariables typeConstructorName

-- | Make a printer for a data type, just the expression part.
dataTypePresenterBody
  :: TypeConstructor -> DataType -> TH.Q TH.Exp
dataTypePresenterBody (TypeConstructor typeConstructorName) (DataType typeVariables constructors) =
  TH.lamCaseE (map constructorCase constructors)
  where typeDisplayExpression = typeDisplay typeVariables typeConstructorName
        constructorCase (Constructor (ValueConstructor valueConstructorName) fields) =
          TH.match (TH.conP valueConstructorName (map (return . fieldPattern) indexedFields))
                   (TH.normalB
                      (TH.appE presentationConstructor (TH.listE (map fieldPresenter indexedFields))))
                   []
          where presentationConstructor =
                  if isTuple typeConstructorName
                     then TH.appE (TH.conE 'TupleValue) typeDisplayExpression
                     else TH.appE (TH.appE (TH.conE (if any (isJust . fst) fields &&
                                                        not (null fields)
                                                        then 'RecordValue
                                                        else 'DataValue))
                                           typeDisplayExpression)
                                  (TH.litE (TH.stringL (TH.pprint valueConstructorName)))
                indexedFields = zip (map indexedFieldName [0 ..]) fields
                fieldPattern (indexedName,_) = TH.VarP indexedName
                fieldPresenter (indexedName,(mvalueVariable,normalType)) =
                  addField (TH.appE (TH.appE (TH.varE 'snd)
                                             (expressType typeVariables normalType))
                                    (TH.varE indexedName))
                  where addField =
                          case mvalueVariable of
                            Nothing -> id
                            Just (ValueVariable fieldName) ->
                              \e ->
                                TH.tupE [TH.stringE (TH.pprint fieldName),e]

-- | Generate an expression which displays a data type and its
-- type variables as instantiated.
typeDisplay
  :: [TypeVariable] -> TH.Name -> TH.Q TH.Exp
typeDisplay typeVariables name =
  (applyToVars . TH.litE . TH.stringL . TH.pprint) name
  where applyToVars typeConstructorDisplay
          | null typeVariables = typeConstructorDisplay
          | isTuple name =
            [|("(" ++
               intercalate
                 ","
                 $(TH.listE (map (\typeVariable ->
                                    TH.appE (TH.varE 'fst)
                                            (TH.varE (presentVarName typeVariable)))
                                 typeVariables)) ++
               ")")|]
          | otherwise =
            TH.appE (TH.varE 'unwords)
                    (TH.infixE (Just (TH.listE [typeConstructorDisplay]))
                               (TH.varE '(++))
                               (Just (TH.listE (map (\typeVariable ->
                                                       TH.appE (TH.varE 'parensIfNeeded)
                                                               (TH.appE (TH.varE 'fst)
                                                                        (TH.varE (presentVarName typeVariable))))
                                                    typeVariables))))

-- | Is a name a tuple?
isTuple :: TH.Name -> Bool
isTuple typeConstructorName =
  any ((== typeConstructorName) . snd) tupleConstructors

-- | Add parens to a string if there's a space inside.
parensIfNeeded :: [Char] -> [Char]
parensIfNeeded e =
  if any isSpace e
     then "(" ++ e ++ ")"
     else e

-- | Make a name for an indexed field of a data type constructor.
indexedFieldName :: Integer -> TH.Name
indexedFieldName index = TH.mkName ("indexedField_" ++ show index)

-- | Make a printer for a type-alias. This involves simply proxying to
-- the real printer, whether that's a data type or a primitive, or
-- another type-alias.
typeAliasPresenter
  :: TypeConstructor -> TypeAlias -> TH.Q [TH.Dec]
typeAliasPresenter typeConstructor@(TypeConstructor typeConstructorName) (TypeAlias typeVariables normalType) =
  presentingFunctionDeclaration
    typeConstructor
    typeVariables
    (TH.tupE [TH.litE (TH.stringL (TH.pprint typeConstructorName))
             ,TH.appE (TH.varE 'snd)
                      (expressType typeVariables normalType)])

-- | Make a presenting function.
builtinFunctionDeclaration
  :: TypeConstructor -> TH.Q TH.Exp -> TH.Q [TH.Dec]
builtinFunctionDeclaration typeConstructor body =
  do dec <-
       TH.valD (TH.varP name)
               (TH.normalB body)
               []
     return [dec]
  where name = presentConsName typeConstructor

-- | Make a presenting function.
presentingFunctionDeclaration :: TypeConstructor
                              -> [TypeVariable]
                              -> TH.Q TH.Exp
                              -> TH.Q [TH.Dec]
presentingFunctionDeclaration typeConstructor@(TypeConstructor typeConstructorName) typeVariables body =
  do sig <-
       TH.sigD name
               (TH.forallT
                  (map (\(TypeVariable typeVariable) -> TH.PlainTV typeVariable) typeVariables)
                  (return [])
                  (foldl (\inner (TypeVariable typeVariable) ->
                            let presentTypeVariable =
                                  return (TH.AppT (TH.AppT (TH.TupleT 2)
                                                           (TH.ConT ''String))
                                                  presenter)
                                  where presenter =
                                          TH.AppT (TH.AppT TH.ArrowT (TH.VarT typeVariable))
                                                  (TH.ConT ''Value)
                            in TH.appT (TH.appT TH.arrowT presentTypeVariable) inner)
                         tupleType
                         (reverse typeVariables)))
     dec <-
       if null typeVariables
          then TH.valD (TH.varP name)
                       (TH.normalB body)
                       []
          else TH.funD name
                       [TH.clause (map (\typeVariable ->
                                          TH.varP (presentVarName typeVariable))
                                       typeVariables)
                                  (TH.normalB body)
                                  []]
     return [sig,dec]
  where name = presentConsName typeConstructor
        tupleType =
          ((\string typ -> TH.AppT (TH.AppT (TH.TupleT 2) string) typ) <$>
           TH.conT ''String <*>
           TH.appT (TH.appT TH.arrowT appliedType)
                   (TH.conT ''Value))
        appliedType =
          foldl TH.appT
                (TH.conT typeConstructorName)
                (map (\(TypeVariable typeVariableName) ->
                        TH.varT typeVariableName)
                     typeVariables)

--------------------------------------------------------------------------------
-- Built-in printers

-- | Are the names basically equal, disregarding package id buggerances?
namesBasicallyEqual
  :: TypeConstructor -> TypeConstructor -> Bool
namesBasicallyEqual (TypeConstructor this) (TypeConstructor that) =
  normalize this == normalize that
  where normalize n@(TH.Name name flavour) =
          case flavour of
            TH.NameG _ _ modName -> TH.Name name (TH.NameQ modName)
            _ -> n

-- | Printers for built-in data types with custom representations
-- (think: primitives, tuples, etc.)
builtInPresenters
  :: [(TypeConstructor,[TypeVariable] -> TH.Exp -> TH.Q TH.Exp)]
builtInPresenters =
  concat [listPrinters
         ,integerPrinters
         ,realPrinters
         ,charPrinters
         ,packedStrings
         ,vectorPrinters
         ,pointerPrinters]

-- | Vectors.
vectorPrinters
  :: [(TypeConstructor,[TypeVariable] -> TH.Exp -> TH.Q TH.Exp)]
vectorPrinters =
  [makeVectorPrinter (qualified "Data.Vector" "Vector")
                     (qualified "Data.Vector" "toList")]
  where makeVectorPrinter typeName unpackFunction =
          (TypeConstructor typeName
          ,\(typeVariable:_) automaticPrinter ->
             (let presentVar = TH.varE (presentVarName typeVariable)
              in TH.lamE [TH.varP (presentVarName typeVariable)]
                         [|(let typeString =
                                  $(TH.stringE (TH.pprint typeName)) ++
                                  " " ++ parensIfNeeded (fst $(presentVar))
                            in (typeString
                               ,\xs ->
                                  ChoiceValue
                                    typeString
                                    [("List"
                                     ,ListValue typeString
                                                (map (snd $(presentVar))
                                                     ($(TH.varE unpackFunction) xs)))
                                    ,("Internal",$(return automaticPrinter) xs)]))|]))
        qualified modName term =
          TH.Name (TH.OccName term)
                  (TH.NameQ (TH.ModName modName))

-- | Packed strings; Text, ByteString.
--
-- This function cleverly acccess functions from these packages in the
-- code generation, without actually needing the `present' package to
-- depend on them directly.
--
packedStrings
  :: [(TypeConstructor,a -> TH.Exp -> TH.Q TH.Exp)]
packedStrings =
  [makeStringPrinter (qualified "Data.ByteString.Internal" "ByteString")
                     (qualified "Data.ByteString.Char8" "unpack")
  ,makeStringPrinter (qualified "Data.ByteString.Lazy.Internal" "ByteString")
                     (qualified "Data.ByteString.Lazy.Char8" "unpack")
  ,makeStringPrinter (qualified "Data.Text.Internal" "Text")
                     (qualified "Data.Text" "unpack")
  ,makeStringPrinter (qualified "Data.Text.Internal.Lazy" "Text")
                     (qualified "Data.Text.Lazy" "unpack")]
  where makeStringPrinter typeName unpackFunction =
          (TypeConstructor typeName
          ,\_ internal ->
             [|let typeString = $(TH.stringE (TH.pprint typeName))
               in (typeString
                  ,\xs ->
                     ChoiceValue
                       typeString
                       [("String"
                        ,StringValue typeString
                                     ($(TH.varE unpackFunction) xs))
                       ,("Internal",$(return internal) xs)])|])
        qualified modName term =
          TH.Name (TH.OccName term)
                  (TH.NameQ (TH.ModName modName))

-- | Printers for list-like types.
listPrinters
  :: [(TypeConstructor,[TypeVariable] -> TH.Exp -> TH.Q TH.Exp)]
listPrinters =
  [(TypeConstructor ''[]
   ,\(typeVariable:_) _automaticPrinter ->
      (let presentVar = TH.varE (presentVarName typeVariable)
       in TH.lamE [TH.varP (presentVarName typeVariable)]
                  [|(let typeString = "[" ++ fst $(presentVar) ++ "]"
                     in (typeString
                        ,\xs ->
                           ListValue typeString (map (snd $(presentVar)) xs)))|]))]

-- | Printers for character-like types.
charPrinters
  :: [(TypeConstructor,a -> TH.Exp -> TH.Q TH.Exp)]
charPrinters = map makeCharPrinter [''Char]
  where makeCharPrinter name =
          (TypeConstructor name
          ,\_ automaticPrinter ->
             [|($(TH.stringE (show name))
               ,\c ->
                  ChoiceValue
                    $(TH.stringE (show name))
                    [("Character"
                     ,CharValue $(TH.stringE (show name))
                                (return c))
                    ,("Unicode point",($(intPrinter Nothing name) (ord c)))
                    ,("Internal",$(return automaticPrinter) c)])|])


-- | Printers for pointer types.
pointerPrinters
  :: [(TypeConstructor,[TypeVariable] -> TH.Exp -> TH.Q TH.Exp)]
pointerPrinters = map makePtrPrinter [''Ptr,''ForeignPtr,''FunPtr]
  where makePtrPrinter name =
          (TypeConstructor name
          ,\(typeVariable:_) automaticPrinter ->
             (let presentVar = TH.varE (presentVarName typeVariable)
              in TH.lamE [TH.varP (presentVarName typeVariable)]
                         [|(let typeString =
                                  $(TH.stringE (show name)) ++
                                  " " ++ parensIfNeeded (fst $(presentVar))
                            in (typeString
                               ,\x ->
                                  ChoiceValue
                                    typeString
                                    [("Pointer"
                                     ,IntegerValue typeString
                                                   (show x))
                                    ,("Internal",$(return automaticPrinter) x)]))|]))

-- | Printers for real number types.
realPrinters
  :: [(TypeConstructor,a -> TH.Exp -> TH.Q TH.Exp)]
realPrinters = map makeIntPrinter [''Float,''Double]
  where makeIntPrinter name =
          (TypeConstructor name
          ,\_ automaticPrinter ->
             [|($(TH.stringE (show name))
               ,$(floatingPrinter (Just automaticPrinter)
                                  name))|])

-- | Printers for integral types.
integerPrinters
  :: [(TypeConstructor,a -> TH.Exp -> TH.Q TH.Exp)]
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
          (TypeConstructor name
          ,\_ automaticPrinter ->
             [|($(TH.stringE (show name))
               ,$(intPrinter (Just automaticPrinter)
                             name))|])

-- | Show a rational as x/y.
showRational :: Rational -> String
showRational x = show (numerator x) ++ "/" ++ show (denominator x)

-- | Floating point printer.
floatingPrinter
  :: Maybe TH.Exp -> TH.Name -> TH.Q TH.Exp
floatingPrinter mautomaticPrinter name =
  [|\x ->
      ChoiceValue
        $(TH.stringE (show name))
        $(case mautomaticPrinter of
            Nothing ->
              [|[("Floating"
                 ,IntegerValue $(TH.stringE (show name))
                               (printf "%f" x))
                ,("Show"
                 ,IntegerValue $(TH.stringE (show name))
                               (show x))
                ,("Rational"
                 ,IntegerValue $(TH.stringE (show name))
                               (showRational (toRational x)))]|]
            Just automaticPrinter ->
              [|[("Floating"
                 ,IntegerValue $(TH.stringE (show name))
                               (printf "%f" x))
                ,("Show"
                 ,IntegerValue $(TH.stringE (show name))
                               (show x))
                ,("Rational"
                 ,IntegerValue $(TH.stringE (show name))
                               (showRational (toRational x)))
                ,("Internal",$(return automaticPrinter) x)]|])|]

-- | Integer printer.
intPrinter
  :: Maybe TH.Exp -> TH.Name -> TH.Q TH.Exp
intPrinter mautomaticPrinter name =
  [|\x ->
      ChoiceValue
        $(TH.stringE (show name))
        $(case mautomaticPrinter of
            Nothing ->
              [|[("Decimal"
                 ,IntegerValue $(TH.stringE (show name))
                               (show x))
                ,("Hexadecimal"
                 ,IntegerValue $(TH.stringE (show name))
                               (Text.Printf.printf "%x" x))
                ,("Binary"
                 ,IntegerValue $(TH.stringE (show name))
                               (Text.Printf.printf "%b" x))]|]
            Just automaticPrinter ->
              [|[("Decimal"
                 ,IntegerValue $(TH.stringE (show name))
                               (show x))
                ,("Hexadecimal"
                 ,IntegerValue $(TH.stringE (show name))
                               (Text.Printf.printf "%x" x))
                ,("Binary"
                 ,IntegerValue $(TH.stringE (show name))
                               (Text.Printf.printf "%b" x))
                ,("Internal",$(return automaticPrinter) x)]|])|]

--------------------------------------------------------------------------------
-- Type Expression
--
-- Given a type, we generate an expression capable of printing that
-- type. It's just a simple translation from type application to
-- function application.
--

-- | Make an expression for presenting a type. This doesn't actually
-- do any unpacking of the data structures pertaining to the types,
-- but rather makes calls to the functions that do.
expressType
  :: [TypeVariable] -> NormalType -> TH.Q TH.Exp
expressType = go 0
  where go arity typeVariables =
          \case
            NormalVar ty ->
              if elem ty typeVariables
                 then TH.varE (presentVarName ty)
                 else return (presentUnknownVar ty arity)
            NormalCons cons -> TH.varE (presentConsName cons)
            NormalPrimitive (PrimitiveTypeConstructor typeConstructorName) ->
              expressPrimitive typeConstructorName
            NormalFunction ty ->
              return (TH.TupE [TH.LitE (TH.StringL (TH.pprint ty))
                              ,TH.LamE [TH.WildP]
                                       (TH.AppE (TH.ConE 'FunctionValue)
                                                (TH.LitE (TH.StringL (TH.pprint ty))))])
            NormalApp f args ->
              foldl TH.appE
                    (go (length args) typeVariables f)
                    (map (go 0 typeVariables) args)

-- | Express a primitive printer.
expressPrimitive :: TH.Name -> TH.Q TH.Exp
expressPrimitive typeConstructorName =
  do info <- TH.reify typeConstructorName
     case info of
       TH.PrimTyConI _ arity _unlifted ->
         return (ignoreTypeVariables
                   arity
                   (TH.TupE [TH.LitE (TH.StringL (TH.pprint typeConstructorName))
                            ,TH.LamE [TH.WildP]
                                     (TH.AppE (TH.ConE 'PrimitiveValue)
                                              (TH.LitE (TH.StringL (TH.pprint typeConstructorName))))]))
       _ -> fail ("Mistaken primitive type: " ++ TH.pprint typeConstructorName)

-- | Name for a function name for presenting a type variable of a data
-- type.
presentUnknownVar
  :: TypeVariable -> Int -> TH.Exp
presentUnknownVar (TypeVariable ty) arity =
  ignoreTypeVariables
    arity
    (TH.TupE [TH.LitE (TH.StringL (TH.pprint ty))
             ,TH.LamE [TH.WildP]
                      (TH.AppE (TH.ConE 'TypeVariableValue)
                               (TH.LitE (TH.StringL (TH.pprint ty))))])

-- | Given the arity, make a lambda of that arity and ignore all the
-- paramters.
ignoreTypeVariables :: Int -> TH.Exp -> TH.Exp
ignoreTypeVariables arity
  | arity == 0 = id
  | otherwise = TH.ParensE . TH.LamE (replicate arity TH.WildP)

-- | Name for a function name for presenting a type variable of a data
-- type.
presentVarName :: TypeVariable -> TH.Name
presentVarName (TypeVariable ty) =
  TH.mkName ("presentVar_" ++ normalizeName ty)

-- | Name for a function name for presenting a type constructor.
presentConsName :: TypeConstructor -> TH.Name
presentConsName (TypeConstructor ty) =
  TH.mkName ("presentCons_" ++ normalizeName ty)

-- | Normalize a name into a regular format.
normalizeName :: TH.Name -> String
normalizeName x = concatMap replace (show x)
  where replace 'z' = "zz"
        replace c
          | isAlphaNum c = [c]
          | otherwise = "z" ++ printf "%x" (ord c)

--------------------------------------------------------------------------------
-- Extension classes
--
-- Some user-defined data structures might have some specific opaque
-- representations, so having some extension classes for a few of them
-- allows us to provide altnerative representations. If such instances
-- are provided, they will be prefered above the other default
-- printers.

-- | Get a mapping from type to instance methods of instances of
-- Present, Present1, etc.
getPresentInstances
  :: TH.Q [(TypeConstructor,ValueVariable)]
getPresentInstances =
  do p0 <- getFor ''Present0
     p1 <- getFor ''Present1
     p2 <- getFor ''Present2
     p3 <- getFor ''Present3
     p4 <- getFor ''Present4
     return (concat [p0,p1,p2,p3,p4])
  where getFor cls =
          do result <- TH.reify cls
             case result of
               TH.ClassI (TH.ClassD _ _ _ _ [TH.SigD method _]) instances ->
                 return (mapMaybe (\i ->
                                     case i of
                                       TH.InstanceD _moverlap _ (TH.AppT (TH.ConT _className) (TH.ConT typeName)) _ ->
                                         Just (TypeConstructor typeName
                                              ,ValueVariable method)
                                       _ -> Nothing)
                                  instances)
               _ -> return []

class Present0 a  where
  present0 :: (String,a -> Value)

class Present1 a where
  present1
    :: (String,x -> Value)
    -> (String,a x -> Value)

class Present2 a where
  present2
    :: (String,x -> Value)
    -> (String,y -> Value)
    -> (String,a x y -> Value)

class Present3 a where
  present3
    :: (String,x -> Value)
    -> (String,y -> Value)
    -> (String,z -> Value)
    -> (String,a x y z -> Value)

class Present4 a where
  present4
    :: (String,x -> Value)
    -> (String,y -> Value)
    -> (String,z -> Value)
    -> (String,z0 -> Value)
    -> (String,a x y z z0 -> Value)

class Present5 a where
  present5
    :: (String,x -> Value)
    -> (String,y -> Value)
    -> (String,z -> Value)
    -> (String,z0 -> Value)
    -> (String,z1 -> Value)
    -> (String,a x y z z0 z1 -> Value)

class Present6 a where
  present6
    :: (String,x -> Value)
    -> (String,y -> Value)
    -> (String,z -> Value)
    -> (String,z0 -> Value)
    -> (String,z1 -> Value)
    -> (String,z2 -> Value)
    -> (String,a x y z z0 z1 z2 -> Value)

--------------------------------------------------------------------------------
-- Actual Presenting
--
-- Finally, we take the type of `it' and generate a set of presenters
-- for it and present the value in a self-contained let-expression.

-- | Present whatever in scope is called `it'
presentIt :: TH.Q TH.Exp
presentIt = presentName (TH.mkName "it")

-- | Make a presenter for the name
presentName :: TH.Name -> TH.Q TH.Exp
presentName name =
  do result <- tryQ (TH.reify name)
     case result of
       Nothing -> fail "Name `it' isn't in scope."
       Just (TH.VarI _ ty _) -> TH.appE (presentType (return ty))
                                          (TH.varE name)
       _ -> fail "The name `it' isn't a variable."
  where tryQ m =
          TH.recover (pure Nothing)
                     (fmap Just m)

-- | Present the value with the given type.
presentType :: TH.Q TH.Type -> TH.Q TH.Exp
presentType getTy =
  do ty <- getTy
     let normalizeResult = normalizeType ty
     case normalizeResult of
       Left err -> fail err
       Right normalType ->
         do instances <- getPresentInstances
            typeDefinitions <- normalTypeDefinitions normalType
            presenters <-
              mapM (typeDefinitionPresenter instances) typeDefinitions
            TH.letE (map return (concat presenters))
                    (TH.infixE (Just (TH.varE 'wrapExceptions))
                               (TH.varE '(.))
                               (Just (TH.appE (TH.varE 'snd)
                                              (expressType [] normalType))))

--------------------------------------------------------------------------------
-- Debugging helpers

-- | Present a value and then use 'toShow' on it.
--
-- >>> :t $(presentShow [t|Maybe Int|])
-- $(presentShow [t|Maybe Int|]) :: Maybe Int -> String
presentShow :: TH.Q TH.Type -> TH.Q TH.Exp
presentShow ty = [|toShow False . $(presentType ty)|]

--------------------------------------------------------------------------------
-- Exception handling
--
-- We want to be able to handle exceptions ("bottom") in data
-- structures, which is particular to Haskell, by returning that as a
-- presentation, too. So instead of failing to present a data
-- structure just because it has _|_ in it, let's instead put an
-- ExceptionValue inside it that can be presented to the user
-- in a sensible manner.

-- | Wrap any _|_ in the presentation with an exception handler.
wrapExceptions :: Value -> Value
wrapExceptions = wrap . go
  where wrap =
          either (\(SomeException exception) ->
                    ExceptionValue (show (typeOf exception))
                                   (show exception))
                 id .
          trySpoon
        go =
          \case
            DataValue a b ps ->
              DataValue a
                            b
                            (map wrapExceptions ps)
            ChoiceValue ty lps ->
              ChoiceValue ty
                          (map (second wrapExceptions) lps)
            RecordValue ty c lps ->
              RecordValue ty
                          c
                          (map (second wrapExceptions) lps)
            ListValue ty ps -> seq ps (ListValue ty (map wrapExceptions ps))
            TupleValue ty ps ->
              seq ps
                  (TupleValue ty
                              (map wrapExceptions ps))
            p@(CharValue _ x) -> seqString p x
            p@(IntegerValue _ x) -> seqString p x
            p@TypeVariableValue{} -> p
            p@PrimitiveValue{} -> p
            p@FunctionValue{} -> p
            p@(StringValue _ x) -> seqString p x
            p@ExceptionValue{} -> p

-- | Seq a string.
seqString :: Value -> String -> Value
seqString = foldl' (\presentation x -> seq x presentation)

-- | Try to get a non-bottom value from the @a@, otherwise return the
-- exception.
trySpoon :: a -> Either SomeException a
trySpoon a = unsafePerformIO (try (evaluate a))

--------------------------------------------------------------------------------
-- Value mediums
--
-- A presentation by itself is useless, it has to be presented in a
-- medium.

-- | To a familiar Show-like string.
toShow :: Bool -> Value -> String
toShow qualified =
  \case
    IntegerValue _ i -> i
    ExceptionValue ex display -> "<" ++ ex ++ ": " ++ show display ++ ">"
    TypeVariableValue ty -> "<_ :: " ++ ty ++ ">"
    CharValue _ c -> "'" ++ c ++ "'"
    FunctionValue ty -> "<" ++ unwords (lines ty) ++ ">"
    DataValue _type name slots ->
      qualify name ++
      (if null slots
          then ""
          else " ") ++
      intercalate " "
                  (map recur slots)
    RecordValue _type name fields ->
      qualify name ++
      " {" ++
      intercalate ","
                  (map showField fields) ++
      "}"
      where showField (fname,slot) =
              qualify fname ++ " = " ++ toShow qualified slot
    TupleValue _type slots ->
      "(" ++
      intercalate ","
                  (map (toShow qualified) slots) ++
      ")"
    ListValue typ slots ->
      if typ == "[GHC.Types.Char]"
         then show (concatMap (\case
                                  CharValue _ c -> c
                                  ChoiceValue _ ((_,CharValue _ c):_) -> c
                                  _ -> []) slots)
         else "[" ++
              intercalate ","
                          (map (toShow qualified) slots) ++
              "]"
    PrimitiveValue p -> "<" ++ p ++ ">"
    StringValue _ string -> show string
    ChoiceValue ty ((_,x):choices) ->
      case x of
        ExceptionValue{}
          | not (null choices) -> toShow qualified (ChoiceValue ty choices)
        _ -> toShow qualified x
    ChoiceValue _ [] -> "<no presentation choices>"
  where recur p
          | atomic p = toShow qualified p
          | otherwise = "(" ++ toShow qualified p ++ ")"
          where atomic =
                  \case
                    ListValue{} -> True
                    IntegerValue{} -> True
                    CharValue{} -> True
                    StringValue{} -> True
                    ChoiceValue ty ((_,x):xs) ->
                      case x of
                        ExceptionValue{}
                          | not (null xs) -> atomic (ChoiceValue ty xs)
                        _ -> atomic x
                    DataValue _ _ [] -> True
                    PrimitiveValue _ -> True
                    _ -> False
        qualify x =
          if qualified
             then x
             else reverse (takeWhile (/= '.') (reverse x))

-- | A presentation of a value up to WHNF.
data WHNF
  = DataWHNF String String [(String,[Integer])]
  | TypeVariableWHNF String
  | PrimitiveWHNF String
  | FunctionWHNF String
  | CharWHNF String String
  | IntegerWHNF String String
  | ChoiceWHNF String [(String,[Integer])]
  | RecordWHNF String String [(String,String,[Integer])]
  | ListConsWHNF String [Integer] [Integer]
  | ListEndWHNF String
  | StringWHNF String String
  | TupleWHNF String [(String,[Integer])]
  | ExceptionWHNF String String
  deriving (Show)

-- | Produce a presentation of the value to WHNF.
toWHNF :: [Integer] -- ^ Cursor.
       -> Value     -- ^ Value to cursor into.
       -> WHNF      -- ^ A WHNF presentation of the value at @cursor@.
toWHNF = go []
  where go
          :: [Integer] -> [Integer] -> Value -> WHNF
        go stack cursor =
          \case
            DataValue typ name slots ->
              case cursor of
                (slot:subCursor) ->
                  case lookup slot (zip [0 ..] slots) of
                    Nothing -> error "toWHNF: Invalid slot."
                    Just value -> go (push [slot]) subCursor value
                _ ->
                  DataWHNF typ
                           name
                           (zipWith (\index slot ->
                                       (valueType slot,push (cursor ++ [index])))
                                    [0 ..]
                                    slots)
            ChoiceValue ty slots ->
              case cursor of
                (slot:subCursor) ->
                  case lookup slot (zip [0 ..] slots) of
                    Nothing -> error "toWHNF: Invalid slot."
                    Just (_,value) -> go (push [slot]) subCursor value
                _ ->
                  ChoiceWHNF
                    ty
                    (zipWith (\index (string,_) ->
                                (string,push (cursor ++ [index])))
                             [0 ..]
                             slots)
            RecordValue typ name slots ->
              case cursor of
                (slot:subCursor) ->
                  case lookup slot (zip [0 ..] slots) of
                    Nothing -> error "toWHNF: Invalid slot."
                    Just (_,value) -> go (push [slot]) subCursor value
                _ ->
                  RecordWHNF
                    typ
                    name
                    (zipWith (\index (fname,slot) ->
                                (valueType slot,fname,push (cursor ++ [index])))
                             [0 ..]
                             slots)
            ListValue ty slots ->
              case cursor of
                (slot:subCursor) ->
                  case slot of
                    0 ->
                      case slots of
                        (value0:_) -> go (push [slot]) subCursor value0
                        _ -> ListEndWHNF ty
                    _ ->
                      case slots of
                        (_:value1) ->
                          go (push [slot])
                             subCursor
                             (ListValue ty value1)
                        _ -> ListEndWHNF ty
                _ ->
                  case slots of
                    [] -> ListEndWHNF ty
                    (_:_) ->
                      ListConsWHNF ty
                                   (push cursor ++ [0])
                                   (push cursor ++ [1])
            TupleValue ty slots ->
              case cursor of
                (slot:subCursor) ->
                  case lookup slot (zip [0 ..] slots) of
                    Nothing -> error "toWHNF: Invalid slot."
                    Just value -> go (push [slot]) subCursor value
                _ ->
                  TupleWHNF ty
                            (zipWith (\index slot ->
                                        (valueType slot
                                        ,push (cursor ++ [index])))
                                     [0 ..]
                                     slots)
            TypeVariableValue ty -> TypeVariableWHNF ty
            PrimitiveValue ty -> PrimitiveWHNF ty
            FunctionValue ty -> FunctionWHNF ty
            CharValue ty ch -> CharWHNF ty ch
            IntegerValue ty rep -> IntegerWHNF ty rep
            StringValue ty str -> StringWHNF ty str
            ExceptionValue ty c -> ExceptionWHNF ty c
          where push xs = stack ++ xs

-- | Get the type of a value.
valueType :: Value -> String
valueType =
  \case
     DataValue ty _ _ -> ty
     TypeVariableValue ty -> ty
     PrimitiveValue ty -> ty
     FunctionValue ty -> ty
     CharValue ty _ -> ty
     IntegerValue ty _ -> ty
     ChoiceValue ty _ -> ty
     RecordValue ty _ _ -> ty
     ListValue ty _ -> ty
     StringValue ty _ -> ty
     TupleValue ty _ -> ty
     ExceptionValue ty _ -> ty

-- | Make JSON from WNHF.
whnfJson :: WHNF -> String
whnfJson =
  \case
    DataWHNF ty name slots ->
      jsonObject
        [("constructor",jsonString "data")
        ,("type",jsonString ty)
        ,("name",jsonString name)
        ,("slots"
         ,jsonList (map (\(typ,sid) ->
                           jsonObject
                             [("type",jsonString typ)
                             ,("id",jsonList (map jsonInteger sid))])
                        slots))]
    TypeVariableWHNF var ->
      jsonObject
        [("constructor",jsonString "type-variable"),("type",jsonString var)]
    PrimitiveWHNF name ->
      jsonObject
        [("constructor",jsonString "primitive"),("type",jsonString name)]
    FunctionWHNF ty ->
      jsonObject [("constructor",jsonString "primitive"),("type",jsonString ty)]
    CharWHNF ty string ->
      jsonObject
        [("constructor",jsonString "char")
        ,("type",jsonString ty)
        ,("string",jsonString string)]
    IntegerWHNF ty string ->
      jsonObject
        [("constructor",jsonString "integer")
        ,("type",jsonString ty)
        ,("string",jsonString string)]
    ChoiceWHNF ty slots ->
      jsonObject
        [("constructor",jsonString "choice")
        ,("type",jsonString ty)
        ,("slots"
         ,jsonList (map (\(typ,sid) ->
                           jsonObject
                             [("title",jsonString typ)
                             ,("id",jsonList (map jsonInteger sid))])
                        slots))]
    RecordWHNF ty name slots ->
      jsonObject
        [("constructor",jsonString "record")
        ,("type",jsonString ty)
        ,("name",jsonString name)
        ,("slots"
         ,jsonList (map (\(typ,name',sid) ->
                           jsonObject
                             [("type",jsonString typ)
                             ,("name",jsonString name')
                             ,("id",jsonList (map jsonInteger sid))])
                        slots))]
    ListConsWHNF typ x xs ->
      jsonObject
        [("constructor",jsonString "list-cons")
        ,("type",jsonString typ)
        ,("car",jsonList (map jsonInteger x))
        ,("cdr",jsonList (map jsonInteger xs))]
    ListEndWHNF typ ->
      jsonObject [("constructor",jsonString "list-end"),("type",jsonString typ)]
    StringWHNF typ string ->
      jsonObject
        [("constructor",jsonString "string")
        ,("type",jsonString typ)
        ,("string",jsonString string)]
    TupleWHNF ty slots ->
      jsonObject
        [("constructor",jsonString "tuple")
        ,("type",jsonString ty)
        ,("slots"
         ,jsonList (map (\(typ,sid) ->
                           jsonObject
                             [("type",jsonString typ)
                             ,("id",jsonList (map jsonInteger sid))])
                        slots))]
    ExceptionWHNF typ shown ->
      jsonObject
        [("constructor",jsonString "exception")
        ,("type",jsonString typ)
        ,("string",jsonString shown)]
  where jsonString :: String -> String
        jsonString = (\x -> "\"" ++ x ++ "\"") . go
          where go s1 =
                  case s1 of
                    (x:xs)
                      | x < '\x20' ->
                        '\\' :
                        encControl x
                                   (go xs)
                    ('"':xs) -> '\\' : '"' : go xs
                    ('\\':xs) -> '\\' : '\\' : go xs
                    (x:xs) -> x : go xs
                    "" -> ""
                encControl x xs =
                  case x of
                    '\b' -> 'b' : xs
                    '\f' -> 'f' : xs
                    '\n' -> 'n' : xs
                    '\r' -> 'r' : xs
                    '\t' -> 't' : xs
                    _
                      | x < '\x10' -> 'u' : '0' : '0' : '0' : hexxs
                      | x < '\x100' -> 'u' : '0' : '0' : hexxs
                      | x < '\x1000' -> 'u' : '0' : hexxs
                      | otherwise -> 'u' : hexxs
                      where hexxs = showHex (fromEnum x) xs
        jsonObject fields =
          "{" ++
          intercalate ", "
                      (map makeField fields) ++
          "}"
          where makeField (name,value) = jsonString name ++ ": " ++ value
        jsonList xs = "[" ++ intercalate ", " xs ++ "]"
        jsonInteger :: Integer -> String
        jsonInteger = show
