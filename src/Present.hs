{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-- | Generate presentations for types.

module Present
  (-- * Presenting functions
   presentIt
  ,makePresenterFor
  ,makeTypePresenter
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

import Control.Arrow
import Control.Exception
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Data (Data)
import Data.Int
import Data.List
import Data.Maybe
import Data.String
import Data.Typeable
import Data.Word
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Text.Printf

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
  TypeVariable Name
  deriving (Show,Eq,Ord)

-- | A type constructor.
newtype TypeConstructor =
  TypeConstructor Name
  deriving (Show,Eq,Ord)

-- | A primitive type constructor.
newtype PrimitiveTypeConstructor =
  PrimitiveTypeConstructor Name
  deriving (Show,Eq,Ord)

-- | A normalized type.
data NormalType
  = NormalCons TypeConstructor
  | NormalPrimitive PrimitiveTypeConstructor
  | NormalFunction Type
  | NormalVar TypeVariable
  | NormalApp NormalType
              [NormalType]
  deriving (Show,Eq,Ord)

-- | Convert the heterogenous TH type into a more normal form.
normalizeType :: Type -> Either String NormalType
normalizeType = go
  where go =
          \case
            ty@AppT{} ->
              do let (typeFunction,typeArguments) = flattenApplication ty
                 case typeFunction of
                   ArrowT -> return (NormalFunction ty)
                   _ -> NormalApp <$> go typeFunction <*> mapM go typeArguments
            ForallT _ context ty ->
              if isFunction ty
                 then return (NormalFunction ty)
                 else go (typeClassDefaulting context ty)
            SigT ty _kind -> go ty
            VarT name -> return (NormalVar (TypeVariable name))
            ConT name ->
              return (if isPrimitiveType name
                         then NormalPrimitive (PrimitiveTypeConstructor name)
                         else NormalCons (TypeConstructor name))
            TupleT i ->
              case lookup i tupleConstructors of
                Nothing -> fail ("Tuple arity " ++ show i ++ " not supported.")
                Just cons -> return (NormalCons (TypeConstructor cons))
            ListT -> return (NormalCons (TypeConstructor ''[]))
            PromotedT _ -> fail "Promoted types are not supported."
            UnboxedTupleT _ -> fail "Unboxed tuples are not supported."
            ArrowT -> fail "The function arrow (->) is not supported."
            EqualityT -> fail "Equalities are not supported."
            PromotedTupleT _ -> fail "Promoted types are not supported."
            PromotedNilT -> fail "Promoted types are not supported."
            PromotedConsT -> fail "Promoted types are not supported."
            StarT -> fail "Star (*) is not supported."
            ConstraintT -> fail "Constraints are not supported."
            LitT _ -> fail "Type-level literals are not supported."

-- | Is the type a function?
isFunction :: Type -> Bool
isFunction ty =
  let (typeFunction,_) = flattenApplication ty
  in case typeFunction of
       ArrowT -> True
       _ -> False

-- | Arity-constructor mapping for tuples.
tupleConstructors :: [(Int,Name)]
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
isPrimitiveType :: Name -> Bool
isPrimitiveType (Name (OccName _) (NameG TcClsName (PkgName "ghc-prim") (ModName "GHC.Prim"))) =
  True
isPrimitiveType name = isSuffixOf "#" (show name)

-- | Flatten a type application f x y into (f,[x,y]).
flattenApplication :: Type -> (Type,[Type])
flattenApplication = go []
  where go args (AppT f x) = go (x : args) f
        go args f = (f,args)

--------------------------------------------------------------------------------
-- Defaulting
--
-- For some classes like Num and IsString, we can default to a
-- reasonable value in the REPL. It leads to a better user-experience.

-- | Apply defaulted substitutions for each of the constraints in the
-- type.
typeClassDefaulting :: [Type] -> Type -> Type
typeClassDefaulting constraints =
  applyTypeSubstitution
    (mapMaybe (\case
                 AppT (ConT className) (VarT varName) ->
                   fmap (\tyName -> (varName,ConT tyName))
                        (lookup className defaultedClasses)
                 _ -> Nothing)
              constraints)

-- | Apply the given substitutions to the type.
applyTypeSubstitution :: [(Name,Type)] -> Type -> Type
applyTypeSubstitution subs = go
  where go =
          \case
            ForallT vars ctx ty -> ForallT vars ctx (go ty)
            AppT f x ->
              AppT (go f)
                   (go x)
            SigT ty k -> SigT (go ty) k
            VarT a
              | Just b <- lookup a subs -> b
              | otherwise -> VarT a
            x -> x

-- | Classes which when encountered in a forall context should have
-- their corresponding type variables substituted on the right hand
-- side with the given type.
defaultedClasses :: [(Name,Name)]
defaultedClasses =
  [(''Integral,''Integer)
  ,(''Num,''Integer)
  ,(''Data,''())
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
  ValueVariable Name
  deriving (Show,Eq,Ord)

-- | Name of a value constructor.
newtype ValueConstructor =
  ValueConstructor Name
  deriving (Show,Eq,Ord)

-- | A normalize representation of a constructor. Present's main
-- algorithm doesn't particularly care whether it's infix, a record,
-- or whatever.
data Constructor =
  Constructor {constructorName :: ValueConstructor
              ,constructorFields :: [(Maybe ValueVariable,NormalType)]}
  deriving (Show,Eq,Ord)

-- | A data type.
data DataType =
  DataType {dataTypeVariables :: [TypeVariable]
           ,dataTypeConstructors :: [Constructor]}
  deriving (Show,Eq,Ord)

-- | A type alias.
data TypeAlias =
  TypeAlias {aliasVariables :: [TypeVariable]
            ,aliasType :: NormalType}
  deriving (Show,Eq,Ord)

-- | Definition of a type.
data TypeDefinition
  = DataTypeDefinition TypeConstructor DataType
  | TypeAliasDefinition TypeConstructor TypeAlias
  deriving (Show,Eq,Ord)

-- | Reify all the constructors of a name. Unless it's primitive, in
-- which case return nothing.
reifyTypeDefinition
  :: TypeConstructor -> Q (Maybe TypeDefinition)
reifyTypeDefinition typeConstructor@(TypeConstructor name) =
  do info <- reify name
     let result =
           case info of
             TyConI dec ->
               case dec of
                 DataD _cxt _ vars cons _deriving ->
                   do cs <- mapM makeConstructor cons
                      return (Just (DataTypeDefinition typeConstructor
                                                       (DataType (map toTypeVariable vars) cs)))
                 NewtypeD _cxt _ vars con _deriving ->
                   do c <- makeConstructor con
                      return (Just (DataTypeDefinition
                                      typeConstructor
                                      (DataType (map toTypeVariable vars)
                                                [c])))
                 TySynD _ vars ty ->
                   do ty' <- normalizeType ty
                      return (Just (TypeAliasDefinition typeConstructor
                                                        (TypeAlias (map toTypeVariable vars) ty')))
                 _ -> fail "Not a supported data type declaration."
             PrimTyConI{} -> return Nothing
             FamilyI{} -> fail "Data families not supported yet."
             _ ->
               fail ("Not a supported object, no type inside it: " ++
                     pprint info)
     case result of
       Left err -> fail err
       Right ok -> return ok

-- | Convert a TH type variable to a normalized type variable.
toTypeVariable :: TyVarBndr -> TypeVariable
toTypeVariable =
  \case
    PlainTV t -> TypeVariable t
    KindedTV t _ -> TypeVariable t

-- | Make a normalized constructor from the more complex TH Con.
makeConstructor
  :: Con -> Either String Constructor
makeConstructor =
  \case
    NormalC name slots ->
      Constructor <$> pure (ValueConstructor name) <*> mapM makeSlot slots
    RecC name fields ->
      Constructor <$> pure (ValueConstructor name) <*> mapM makeField fields
    InfixC t1 name t2 ->
      Constructor <$> pure (ValueConstructor name) <*>
      ((\x y -> [x,y]) <$> makeSlot t1 <*> makeSlot t2)
    ForallC _ _ _ -> fail "Existentials aren't supported."
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
  :: NormalType -> Q [TypeDefinition]
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
liftQ :: Q a -> StateT s Q a
liftQ m =
  StateT (\s ->
            do v <- m
               return (v,s))

--------------------------------------------------------------------------------
-- Printer Generation
--
-- Given a TypeDefinition, generate a printer for that data type.

data Presentation
  = DataTypePresentation String
                         String
                         [Presentation]
  | TypeVariablePresentation String
  | PrimitivePresentation String
  | FunctionPresentation String
  | CharPresentation String
                     String
  | IntegerPresentation String
                        String
  | ChoicePresentation String
                       [(String,Presentation)]
  | RecordPresentation String
                       String
                       [(String,Presentation)]
  | ListPresentation String
                     [Presentation]
  | StringPresentation String
                       String
  | TuplePresentation String
                      [Presentation]
  | ExceptionPresentation String
                          String
  deriving (Show,Eq)

-- | Make a presenter for a type definition.
typeDefinitionPresenter :: [(TypeConstructor,ValueVariable)] -> TypeDefinition -> Q [Dec]
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
          do
             instanceBasedPresenter typeConstructor methodName dataType typeVariables
    TypeAliasDefinition typeConstructor typeAlias ->
      typeAliasPresenter typeConstructor typeAlias

-- | Make a printer based on an instance declaration for Present[N].
instanceBasedPresenter :: TypeConstructor
                       -> ValueVariable
                       -> DataType
                       -> [TypeVariable]
                       -> Q [Dec]
instanceBasedPresenter typeConstructor@(TypeConstructor typeConstructorName) (ValueVariable methodName) dataType typeVariables =
  presentingFunctionDeclaration
    typeConstructor
    typeVariables
    (tupE [return typeDisplayExpression
          ,[|\x ->
               ChoicePresentation
                 $(return typeDisplayExpression)
                 [("Instance"
                  ,snd $(foldl appE
                               (varE methodName)
                               (map (varE . presentVarName) typeVariables))
                       x)
                 ,("Internal"
                  ,$(dataTypePresenterBody typeConstructor dataType) x)]|]])
  where typeDisplayExpression = typeDisplay typeVariables typeConstructorName

-- | Make a presenter for the given data type.
dataTypePresenter :: TypeConstructor -> DataType -> Q [Dec]
dataTypePresenter typeConstructor@(TypeConstructor typeConstructorName) dataType@(DataType typeVariables _) =
  presentingFunctionDeclaration
    typeConstructor
    typeVariables
    (tupE [return typeDisplayExpression
          ,dataTypePresenterBody typeConstructor dataType])
  where typeDisplayExpression = typeDisplay typeVariables typeConstructorName

-- | Make a printer for a data type, just the expression part.
dataTypePresenterBody :: TypeConstructor -> DataType -> Q Exp
dataTypePresenterBody (TypeConstructor typeConstructorName) (DataType typeVariables constructors) =
  lamCaseE (map constructorCase constructors)
  where typeDisplayExpression = typeDisplay typeVariables typeConstructorName
        constructorCase (Constructor (ValueConstructor valueConstructorName) fields) =
          match (conP valueConstructorName (map (return . fieldPattern) indexedFields))
                (normalB (appE (appE (appE (conE (if all (isJust . fst) fields
                                                     then 'RecordPresentation
                                                     else 'DataTypePresentation))
                                           (return typeDisplayExpression))
                                     (litE (stringL (pprint valueConstructorName))))
                               (listE (map fieldPresenter indexedFields))))
                []
          where indexedFields = zip (map indexedFieldName [0 ..]) fields
                fieldPattern (indexedName,_) = VarP indexedName
                fieldPresenter (indexedName,(mvalueVariable,normalType)) =
                  addField (appE (appE (varE 'snd)
                                       (expressType typeVariables normalType))
                                 (varE indexedName))
                  where addField =
                          case mvalueVariable of
                            Nothing -> id
                            Just (ValueVariable fieldName) ->
                              \e -> tupE [stringE (pprint fieldName),e]

-- | Generate an expression which displays a data type and its
-- type variables as instantiated.
typeDisplay :: Ppr a => [TypeVariable] -> a -> Exp
typeDisplay typeVariables = applyToVars . LitE . StringL . pprint
  where applyToVars typeConstructorDisplay
          | null typeVariables = typeConstructorDisplay
          | otherwise =
            AppE (VarE 'unwords)
                 (InfixE (Just (ListE [typeConstructorDisplay]))
                         (VarE '(++))
                         (Just (ListE (map (\typeVariable ->
                                              AppE (VarE 'parensIfNeeded)
                                                   (AppE (VarE 'fst)
                                                         (VarE (presentVarName typeVariable))))
                                           typeVariables))))

-- | Add parens to a string if there's a space inside.
parensIfNeeded :: [Char] -> [Char]
parensIfNeeded e =
  if any isSpace e
     then "(" ++ e ++ ")"
     else e

-- | Make a name for an indexed field of a data type constructor.
indexedFieldName :: Integer -> Name
indexedFieldName index = mkName ("indexedField_" ++ show index)

-- | Make a printer for a type-alias. This involves simply proxying to
-- the real printer, whether that's a data type or a primitive, or
-- another type-alias.
typeAliasPresenter :: TypeConstructor -> TypeAlias -> Q [Dec]
typeAliasPresenter typeConstructor@(TypeConstructor typeConstructorName) (TypeAlias typeVariables normalType) =
  presentingFunctionDeclaration
    typeConstructor
    typeVariables
    (tupE [litE (stringL (pprint typeConstructorName))
          ,appE (varE 'snd)
                (expressType typeVariables normalType)])

-- | Make a presenting function.
builtinFunctionDeclaration :: TypeConstructor -> Q Exp -> Q [Dec]
builtinFunctionDeclaration typeConstructor body =
  do dec <-
       valD (varP name)
            (normalB body)
            []
     return [dec]
  where name = presentConsName typeConstructor

-- | Make a presenting function.
presentingFunctionDeclaration :: TypeConstructor -> [TypeVariable] -> Q Exp -> Q [Dec]
presentingFunctionDeclaration typeConstructor@(TypeConstructor typeConstructorName) typeVariables body =
  do sig <-
       sigD name
            (forallT (map (\(TypeVariable typeVariable) -> PlainTV typeVariable) typeVariables)
                     (return [])
                     (foldl (\inner (TypeVariable typeVariable) ->
                               let presentTypeVariable =
                                     return (AppT (AppT (TupleT 2)
                                                        (ConT ''String))
                                                  presenter)
                                     where presenter =
                                             AppT (AppT ArrowT (VarT typeVariable))
                                                  (ConT ''Presentation)
                               in appT (appT arrowT presentTypeVariable) inner)
                            tupleType
                            (reverse typeVariables)))
     dec <-
       if null typeVariables
          then valD (varP name)
                    (normalB body)
                    []
          else funD name
                    [clause (map (\typeVariable ->
                                    varP (presentVarName typeVariable))
                                 typeVariables)
                            (normalB body)
                            []]
     return [sig,dec]
  where name = presentConsName typeConstructor
        tupleType =
          ((\string typ -> AppT (AppT (TupleT 2) string) typ) <$> conT ''String <*>
           appT (appT arrowT appliedType)
                (conT ''Presentation))
        appliedType =
          foldl appT
                (conT typeConstructorName)
                (map (\(TypeVariable typeVariableName) -> varT typeVariableName) typeVariables)

--------------------------------------------------------------------------------
-- Built-in printers

-- | Are the names basically equal, disregarding package id buggerances?
namesBasicallyEqual :: TypeConstructor -> TypeConstructor -> Bool
namesBasicallyEqual (TypeConstructor this) (TypeConstructor that) =
  normalize this == normalize that
  where normalize n@(Name name flavour) =
          case flavour of
            NameG _ _ modName -> Name name (NameQ modName)
            _ -> n

-- | Printers for built-in data types with custom representations
-- (think: primitives, tuples, etc.)
builtInPresenters :: [(TypeConstructor,[TypeVariable] -> Exp -> Q Exp)]
builtInPresenters =
  concat [listPrinters
         ,integerPrinters
         ,charPrinters
         ,packedStrings
         ,vectorPrinters]

-- | Vectors.
vectorPrinters :: [(TypeConstructor,[TypeVariable] -> Exp -> Q Exp)]
vectorPrinters =
  [makeVectorPrinter (qualified "Data.Vector" "Vector")
                     (qualified "Data.Vector" "toList")]
  where makeVectorPrinter typeName unpackFunction =
          (TypeConstructor typeName
          ,\(typeVariable:_) automaticPrinter ->
             (let presentVar = varE (presentVarName typeVariable)
              in lamE [varP (presentVarName typeVariable)]
                      [|(let typeString =
                               $(stringE (pprint typeName)) ++
                               " " ++ parensIfNeeded (fst $(presentVar))
                         in (typeString
                            ,\xs ->
                               ChoicePresentation
                                 typeString
                                 [("List"
                                  ,ListPresentation
                                     typeString
                                     (map (snd $(presentVar))
                                          ($(varE unpackFunction) xs)))
                                 ,("Internal",$(return automaticPrinter) xs)]))|]))
        qualified modName term =
          Name (OccName term)
               (NameQ (ModName modName))

-- | Packed strings; Text, ByteString.
--
-- This function cleverly acccess functions from these packages in the
-- code generation, without actually needing the `present' package to
-- depend on them directly.
--
packedStrings :: [(TypeConstructor,a -> Exp -> Q Exp)]
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
             [|let typeString = $(stringE (pprint typeName))
               in (typeString
                  ,\xs ->
                     ChoicePresentation
                       typeString
                       [("String"
                        ,StringPresentation typeString
                                            ($(varE unpackFunction) xs))
                       ,("Internal",$(return internal) xs)])|])
        qualified modName term =
          Name (OccName term)
               (NameQ (ModName modName))

-- | Printers for list-like types.
listPrinters :: [(TypeConstructor, [TypeVariable] -> Exp -> Q Exp)]
listPrinters =
  [(TypeConstructor ''[]
   ,\(typeVariable:_) _automaticPrinter ->
      (let presentVar = varE (presentVarName typeVariable)
       in lamE [varP (presentVarName typeVariable)]
               [|(let typeString = "[" ++ fst $(presentVar) ++ "]"
                  in (typeString
                     ,\xs ->
                        case fst $(presentVar) of
                          "GHC.Types.Char" ->
                            ChoicePresentation
                              "String"
                              [("String"
                               ,StringPresentation "String"
                                                   (concatMap getCh (map (snd $(presentVar)) xs)))
                              ,("List of characters"
                               ,ListPresentation typeString
                                                 (map (snd $(presentVar)) xs))]
                            where getCh (CharPresentation "GHC.Types.Char" ch) =
                                    ch
                                  getCh (ChoicePresentation _ ((_,CharPresentation _ ch):_)) =
                                    ch
                                  getCh _ = ""
                          _ ->
                            ListPresentation typeString
                                             (map (snd $(presentVar)) xs)))|]))]

-- | Printers for character-like types.
charPrinters :: [(TypeConstructor, a -> Exp -> Q Exp)]
charPrinters = map makeCharPrinter [''Char]
  where makeCharPrinter name =
          (TypeConstructor name
          ,\_ automaticPrinter ->
             [|($(stringE (show name))
               ,\c ->
                  ChoicePresentation
                    $(stringE (show name))
                    [("Character"
                     ,CharPresentation $(stringE (show name))
                                       (return c))
                    ,("Unicode point",($(intPrinter Nothing name) (ord c)))
                    ,("Internal",$(return automaticPrinter) c)])|])

-- | Printers for integral types.
integerPrinters :: [(TypeConstructor, a -> Exp -> Q Exp)]
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
             [|($(stringE (show name))
               ,$(intPrinter (Just automaticPrinter)
                             name))|])

-- | Integer printer.
intPrinter :: Maybe Exp -> Name -> Q Exp
intPrinter mautomaticPrinter name =
  [|\x ->
      ChoicePresentation
        $(stringE (show name))
        $(case mautomaticPrinter of
            Nothing ->
              [|[("Decimal"
                 ,IntegerPresentation $(stringE (show name))
                                      (show x))
                ,("Hexadecimal"
                 ,IntegerPresentation $(stringE (show name))
                                      (Text.Printf.printf "%x" x))
                ,("Binary"
                 ,IntegerPresentation $(stringE (show name))
                                      (Text.Printf.printf "%b" x))]|]
            Just automaticPrinter ->
              [|[("Decimal"
                 ,IntegerPresentation $(stringE (show name))
                                      (show x))
                ,("Hexadecimal"
                 ,IntegerPresentation $(stringE (show name))
                                      (Text.Printf.printf "%x" x))
                ,("Binary"
                 ,IntegerPresentation $(stringE (show name))
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
expressType :: [TypeVariable] -> NormalType -> Q Exp
expressType = go 0
  where go arity typeVariables =
          \case
            NormalVar ty ->
              if elem ty typeVariables
                 then varE (presentVarName ty)
                 else return (presentUnknownVar ty arity)
            NormalCons cons -> varE (presentConsName cons)
            NormalPrimitive (PrimitiveTypeConstructor typeConstructorName) ->
              expressPrimitive typeConstructorName
            NormalFunction ty ->
              return (TupE [LitE (StringL (pprint ty))
                           ,LamE [WildP]
                                 (AppE (ConE 'FunctionPresentation)
                                       (LitE (StringL (pprint ty))))])
            NormalApp f args ->
              foldl appE
                    (go (length args) typeVariables f)
                    (map (go 0 typeVariables) args)

-- | Express a primitive printer.
expressPrimitive :: Name -> Q Exp
expressPrimitive typeConstructorName =
  do info <- reify typeConstructorName
     case info of
       PrimTyConI _ arity _unlifted ->
         return (ignoreTypeVariables
                   arity
                   (TupE [LitE (StringL (pprint typeConstructorName))
                         ,LamE [WildP]
                               (AppE (ConE 'PrimitivePresentation)
                                     (LitE (StringL (pprint typeConstructorName))))]))
       _ -> fail ("Mistaken primitive type: " ++ pprint typeConstructorName)

-- | Name for a function name for presenting a type variable of a data
-- type.
presentUnknownVar :: TypeVariable -> Int -> Exp
presentUnknownVar (TypeVariable ty) arity =
  ignoreTypeVariables
    arity
    (TupE [LitE (StringL (pprint ty))
          ,LamE [WildP]
                (AppE (ConE 'TypeVariablePresentation)
                      (LitE (StringL (pprint ty))))])

-- | Given the arity, make a lambda of that arity and ignore all the
-- paramters.
ignoreTypeVariables :: Int -> Exp -> Exp
ignoreTypeVariables arity
  | arity == 0 = id
  | otherwise = ParensE . LamE (replicate arity WildP)

-- | Name for a function name for presenting a type variable of a data
-- type.
presentVarName :: TypeVariable -> Name
presentVarName (TypeVariable ty) = mkName ("presentVar_" ++ normalizeName ty)

-- | Name for a function name for presenting a type constructor.
presentConsName :: TypeConstructor -> Name
presentConsName (TypeConstructor ty) = mkName ("presentCons_" ++ normalizeName ty)

-- | Normalize a name into a regular format.
normalizeName :: Name -> String
normalizeName x = concatMap replace (show x)
  where replace 'z' = "zz"
        replace c | isAlphaNum c = [c]
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
getPresentInstances :: Q [(TypeConstructor,ValueVariable)]
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
                                         Just (TypeConstructor typeName,ValueVariable method)
                                       _ -> Nothing)
                                  instances)
               _ -> return []

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
-- Actual Presenting
--
-- Finally, we take the type of `it' and generate a set of presenters
-- for it and present the value in a self-contained let-expression.

-- | Present whatever in scope is called `it'
presentIt :: Q Exp
presentIt =
  appE (makePresenterFor (mkName "it"))
       (varE (mkName "it"))

-- | Make a presenter for the name
makePresenterFor :: Name -> Q Exp
makePresenterFor name =
  do result <- tryQ (reify name)
     case result of
       Nothing -> fail "Name `it' isn't in scope."
       Just (VarI _ ty _ _) ->
         makeTypePresenter (return ty)
       _ -> fail "The name `it' isn't a variable."
  where tryQ m =
          recover (pure Nothing)
                  (fmap Just m)

-- | Present the value with the given type.
makeTypePresenter :: Q Type -> Q Exp
makeTypePresenter getTy =
  do ty <- getTy
     let normalizeResult = normalizeType ty
     case normalizeResult of
       Left err -> fail err
       Right normalType ->
         do instances <- getPresentInstances
            typeDefinitions <- normalTypeDefinitions normalType
            presenters <- mapM (typeDefinitionPresenter instances) typeDefinitions
            letE (map return (concat presenters))
                 (infixE (Just (varE 'wrapExceptions))
                         (varE '(.))
                         (Just (appE (varE 'snd)
                                     (expressType [] normalType))))

--------------------------------------------------------------------------------
-- Exception handling
--
-- We want to be able to handle exceptions ("bottom") in data
-- structures, which is particular to Haskell, by returning that as a
-- presentation, too. So instead of failing to present a data
-- structure just because it has _|_ in it, let's instead put an
-- ExceptionPresentation inside it that can be presented to the user
-- in a sensible manner.

-- | Wrap any _|_ in the presentation with an exception handler.
wrapExceptions :: Presentation -> Presentation
wrapExceptions = wrap . go
  where wrap =
          either (\(SomeException exception) ->
                    ExceptionPresentation (show (typeOf exception))
                                          (show exception))
                 id .
          trySpoon
        go =
          \case
            DataTypePresentation a b ps ->
              DataTypePresentation a
                                   b
                                   (map wrapExceptions ps)
            ChoicePresentation ty lps ->
              ChoicePresentation ty
                                 (map (second wrapExceptions) lps)
            RecordPresentation ty c lps ->
              RecordPresentation ty
                                 c
                                 (map (second wrapExceptions) lps)
            ListPresentation ty ps ->
              seq ps
                  (ListPresentation ty
                                    (map wrapExceptions ps))
            TuplePresentation ty ps ->
              seq ps
                  (TuplePresentation ty
                                     (map wrapExceptions ps))
            p@(CharPresentation _ x) -> seqString p x
            p@(IntegerPresentation _ x) -> seqString p x
            p@TypeVariablePresentation{} -> p
            p@PrimitivePresentation{} -> p
            p@FunctionPresentation{} -> p
            p@(StringPresentation _ x) -> seqString p x
            p@ExceptionPresentation{} -> p

-- | Seq a string.
seqString :: Presentation -> String -> Presentation
seqString = foldl' (\presentation x -> seq x presentation)

-- | Try to get a non-bottom value from the @a@, otherwise return the
-- exception.
trySpoon :: a -> Either SomeException a
trySpoon a = unsafePerformIO (try (evaluate a))

--------------------------------------------------------------------------------
-- Presentation mediums
--
-- A presentation by itself is useless, it has to be presented in a
-- medium.

-- | To a familiar Show-like string.
toShow :: Bool -> Presentation -> String
toShow qualified =
  \case
    IntegerPresentation _ i -> i
    ExceptionPresentation ex display ->
      "<" ++ ex ++ ": " ++ show display ++ ">"
    TypeVariablePresentation ty -> "<_ :: " ++ ty ++ ">"
    CharPresentation _ c -> "'" ++ c ++ "'"
    FunctionPresentation ty -> "<" ++ unwords (lines ty) ++ ">"
    DataTypePresentation _type name slots ->
      qualify name ++
      (if null slots
          then ""
          else " ") ++
      intercalate " "
                  (map recur slots)
    RecordPresentation _type name fields ->
      qualify name ++
      " {" ++
      intercalate ","
                  (map showField fields) ++
      "}"
      where showField (fname,slot) =
              qualify fname ++ " = " ++ toShow qualified slot
    TuplePresentation _type slots ->
      "(" ++
      intercalate ","
                  (map (toShow qualified) slots) ++
      ")"
    ListPresentation _type slots ->
      "[" ++
      intercalate ","
                  (map (toShow qualified) slots) ++
      "]"
    PrimitivePresentation p -> "<" ++ p ++ ">"
    StringPresentation _ string -> show string
    ChoicePresentation ty ((_,x):choices) ->
      case x of
        ExceptionPresentation{} | not (null choices) ->
          toShow qualified (ChoicePresentation ty choices)
        _ -> toShow qualified x
    ChoicePresentation _ [] -> "<no presentation choices>"
  where recur p
          | atomic p = toShow qualified p
          | otherwise = "(" ++ toShow qualified p ++ ")"
          where atomic =
                  \case
                    ListPresentation{} -> True
                    IntegerPresentation{} -> True
                    CharPresentation{} -> True
                    StringPresentation{} -> True
                    ChoicePresentation ty ((_,x):xs) ->
                      case x of
                        ExceptionPresentation{} | not (null xs) ->
                          atomic (ChoicePresentation ty xs)
                        _ -> atomic x
                    DataTypePresentation _ _ [] -> True
                    PrimitivePresentation _ -> True
                    _ -> False
        qualify x =
          if qualified
             then x
             else reverse (takeWhile (/= '.') (reverse x))
