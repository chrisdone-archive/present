{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Make presentations for data types.

module Present
  (Presentation(..)
  ,ID
  ,present
  ,asData)
  where

import qualified Present.ByteString as P
import           Present.ID (ID)
import qualified Present.ID as ID
import qualified Present.String as P
import qualified Present.Text as P
import           Present.Types

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Data
import           Data.Data.Exists
import           Data.Data.Indexed
import           Data.Default
import           Data.Semigroup
import           Data.Text (pack)

-- | Normalize real types into presentation types.
data Normalizer = forall a b. (Data a,Data b) => Norm (a -> b)

-- | Present the breadth-first level of a data type.
present :: Data a => ID -> a -> Maybe Presentation
present iq = hunt iq def iq

-- | Hunt through the data structure, normalizing special data types
-- like Text and ByteString and String.
hunt :: Data d => ID -> ID -> ID -> d -> Maybe Presentation
hunt iq c q d =
  normalize d normalizers <|> dissect iq c q d
  where
    normalize a = foldr mplus mzero . map (tryNormalize a)
    tryNormalize x (Norm convert) = cast x >>= retry . convert
    retry :: Data r => r -> Maybe Presentation
    retry = hunt iq c q

-- | Normalizers which convert from one more complicated type to a
-- simpler, easier to present type.
normalizers :: [Normalizer]
normalizers =
  [Norm P.normalizeText
  ,Norm P.normalizeStrictText
  ,Norm P.normalizeByteString
  ,Norm P.normalizeStrictByteString
  ,Norm P.normalizeString]

-- | Dissect the actual data structure and find and present the slot
-- we're looking for.
dissect :: Data d => ID -> ID -> ID -> d -> Maybe Presentation
dissect iq c q d =
  case ID.split q of
    (i,Nothing) ->
      if ID.singleton i == c
         then presentation iq d
         else Nothing
    (i,Just q') ->
      if ID.singleton i == c
         then case gindex i' d of
                Nothing -> Nothing
                Just (D d') -> hunt iq (ID.singleton i') q' d'
         else Nothing
         where (i',_) = ID.split q'

-- | Make a presentation for the given data structure.
presentation :: Data a => ID -> a -> Maybe Presentation
presentation iq d =
  case dataTypeRep dtype of
    AlgRep{} -> Just (presentAlgebraic iq d)
    IntRep   -> Just (Integer ty text)
    FloatRep -> Just (Floating ty text)
    CharRep  -> Just (Char ty text)
    NoRep    -> Nothing
  where text = pack (show (toConstr d))
        ty = pack (show (typeOf d))
        dtype = dataTypeOf d

-- | Present an algebraic data type. This catches strings, tuples,
-- lists as separate presentation types, for better display.
presentAlgebraic :: Data a => ID -> a -> Presentation
presentAlgebraic iq d =
  case cast d of
    Just (_ :: P.String) -> String ty ids
    Nothing ->
      case cast d of
        Just (_ :: P.Text) -> String ty ids
        Nothing ->
          case cast d of
            Just (_ :: P.ByteString) -> String ty ids
            Nothing ->
              case show (toConstr d) of
                "[]"  -> List ty []
                "()"  -> Tuple ty []
                "(:)" -> List ty ids
                "(,)" -> Tuple ty ids
                _ ->
                  case constrFields (toConstr d) of
                    [] -> Alg ty text ids
                    fields -> Record ty text (zip (map pack fields) ids)
  where ty = pack (show (typeOf d))
        text = pack (show (toConstr d))
        ids = gappend makeId d
          where makeId d_i i =
                  return (pack (show (typeOf d_i))
                         ,iq <> ID.singleton i)

-- | A helpful function for editors to force that a value is an
-- instance of "Data", before we actually start using it.
asData :: Data a => a -> a
asData = id
