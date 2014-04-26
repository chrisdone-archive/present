-- | Make presentations for data types.

module Present where

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
  deriving (Show)

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
