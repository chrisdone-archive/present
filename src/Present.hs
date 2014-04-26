{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Make presentations for data types.

module Present where

import           Data.Aeson (ToJSON(..),(.=),object)
import           Data.AttoLisp (ToLisp(..),Lisp(Symbol))
import           Data.Data
import           Data.Data.Exists
import           Data.Data.Indexed
import           Data.Default
import           Data.ID (ID)
import qualified Data.ID as ID
import           Data.Semigroup
import           Data.Text (Text,pack,isPrefixOf)

-- | A presentation of a level of a data type.
data Presentation
  = Integer !Text !Text
  | Floating !Text !Text
  | Char !Text !Text
  | Alg !Text !Text ![(Text,ID)]
  | Tuple !Text ![(Text,ID)]
  | List !Text ![(Text,ID)]
  | String !Text ![(Text,ID)]
  deriving (Show,Typeable,Data)

instance ToJSON Presentation where
  toJSON x =
    case x of
      Integer ty i -> object ["rep" .= ("integer" :: Text),"type" .= ty,"text" .= i]
      Floating ty f -> object ["rep" .= ("floating" :: Text),"type" .= ty,"text" .= f]
      Char ty c -> object ["rep" .= ("char" :: Text),"type" .= ty,"text" .= c]
      Alg ty t slots ->
        object ["rep" .= ("alg" :: Text)
               ,"type" .= ty
               ,"text" .= t
               ,"slots" .= toJSON (map toJSON slots)]
      Tuple ty slots ->
        object ["rep" .= ("tuple" :: Text)
               ,"type" .= ty
               ,"slots" .= toJSON (map toJSON slots)]
      List ty slots ->
        object ["rep" .= ("list" :: Text)
               ,"type" .= ty
               ,"slots" .= toJSON (map toJSON slots)]
      String ty slots ->
        object ["rep" .= ("string" :: Text)
               ,"type" .= ty
               ,"slots" .= toJSON (map toJSON slots)]

instance ToLisp Presentation where
  toLisp x =
    case x of
      Integer ty i -> assoc ["rep" .: ("integer" :: Text),"type" .: ty,"text" .: i]
      Floating ty f -> assoc ["rep" .: ("floating" :: Text),"type" .: ty,"text" .: f]
      Char ty c -> assoc ["rep" .: ("char" :: Text),"type" .: ty,"text" .: c]
      Alg ty t slots ->
        assoc ["rep" .: ("alg" :: Text)
              ,"type" .: ty
              ,"text" .: t
              ,"slots" .: toLisp (map toLisp slots)]
      Tuple ty slots ->
        assoc ["rep" .: ("tuple" :: Text)
              ,"type" .: ty
              ,"slots" .: toLisp (map toLisp slots)]
      List ty slots ->
        assoc ["rep" .: ("list" :: Text)
              ,"type" .: ty
              ,"slots" .: toLisp (map toLisp slots)]
      String ty slots ->
        assoc ["rep" .: ("string" :: Text)
              ,"type" .: ty
              ,"slots" .: toLisp (map toLisp slots)]
    where name .: slot = (Symbol name,toLisp slot)
          assoc = toLisp

-- | Present the breadth-first level of a data type.
present :: Data a => ID -> a -> Maybe Presentation
present iq =
  hunt def iq
  where
    hunt :: Data d => ID -> ID -> d -> Maybe Presentation
    hunt c q d =
      case ID.split q of
        (i,Nothing) ->
          if ID.singleton i == c
             then presentation iq d
             else Nothing
        (i,Just q') ->
          if ID.singleton i == c
             then case gindex i' d of
                    Nothing -> Nothing
                    Just (D d') -> hunt (ID.singleton i') q' d'
             else Nothing
             where (i',_) = ID.split q'

-- | Make a presentation for the given data structure.
presentation :: Data a => ID -> a -> Maybe Presentation
presentation iq d =
  case dataTypeRep dtype of
    AlgRep{} ->
      Just (if ty == "[Char]" || ty == "String"
               then String ty ids
               else if isPrefixOf "[" ty
                       then List ty ids
                       else if isPrefixOf "(" ty
                               then Tuple ty ids
                               else Alg ty text ids )
      where text = pack (show (toConstr d))
            ids = gappend (\d i -> return (pack (show (typeOf d))
                                          ,iq <> ID.singleton i))
                          d
    IntRep -> Just (Integer ty text)
    FloatRep -> Just (Floating ty text)
    CharRep -> Just (Char ty text)
    NoRep -> Nothing
  where text = pack (show (toConstr d))
        ty = pack (show (typeOf d))
        dtype = dataTypeOf d

-- | A helpful function for editors to force that a value is an
-- instance of "Data", before we actually start using it.
asData :: Data a => a -> a
asData = id
