{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Presentation types.

module Present.Types
  (Presentation(..))
  where

import Present.ID (ID)

import Data.Aeson (ToJSON(..),(.=),object)
import Data.AttoLisp (ToLisp(..),Lisp(Symbol))
import Data.Data
import Data.Text (Text)

-- | A presentation of a level of a data type.
data Presentation
  = Integer !Text !Text
  | Floating !Text !Text
  | Char !Text !Text
  | Alg !Text !Text ![(Text,ID)]
  | Record !Text !Text ![(Text,(Text,ID))]
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
      Record ty t slots ->
        object ["rep" .= ("record" :: Text)
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
      Record ty t slots ->
        assoc ["rep" .: ("record" :: Text)
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
