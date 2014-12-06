-- | Structured, lazy presentations of Haskell data types for
-- inspection while developing.

module Present where

import Present.Types

import Data.Monoid

-- | Make a presentation at the given cursor.
present :: Present a => Cursor -> a -> Presentation
present = presentValue mempty
