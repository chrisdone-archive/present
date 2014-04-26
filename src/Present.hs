{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Make presentations for data types.

module Present where

import           Data.Aeson
import           Data.AttoLisp
import           Data.Data
import           Data.Data.Exists
import           Data.Data.Indexed
import           Data.Default
import           Data.ID (ID)
import qualified Data.ID as ID
import           Data.Semigroup
import           Data.Text (Text,pack)

-- | A presentation of a level of a data type.
data Presentation
  = Integer !Text
  | Floating !Text
  | Char !Text
  | Alg !Text ![ID]
  deriving (Show,Typeable,Data)

instance ToJSON Presentation where
  toJSON x =
    case x of
      Integer i -> object ["type" .= ("integer" :: Text),"text" .= i]
      Floating f -> object ["type" .= ("floating" :: Text),"text" .= f]
      Char c -> object ["type" .= ("char" :: Text),"text" .= c]
      Alg t slots ->
        object ["type" .= ("alg" :: Text)
               ,"text" .= t
               ,"slots" .= toJSON (map toJSON slots)]

instance ToLisp Presentation where
  toLisp x =
    case x of
      Integer i -> assoc ["type" .: ("integer" :: Text),"text" .: i]
      Floating f -> assoc ["type" .: ("floating" :: Text),"text" .: f]
      Char c -> assoc ["type" .: ("char" :: Text),"text" .: c]
      Alg t slots ->
        assoc ["type" .: ("alg" :: Text)
              ,"text" .: t
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
      Just (Alg (pack (show (toConstr d)))
                (gappend (const (return . (iq <>) . ID.singleton)) d))
    IntRep -> Just (Integer text)
    FloatRep -> Just (Floating text)
    CharRep -> Just (Char text)
    NoRep -> Nothing
  where text = pack (show (toConstr d))
        dtype = dataTypeOf d

data Foo = Foo Bar [Bar] Char Int
  deriving (Typeable,Data)

data Bar = Bar () Bool
  deriving (Typeable,Data)
