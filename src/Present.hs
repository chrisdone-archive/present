-- | Structured, lazy presentations of Haskell data types for
-- inspection while developing.

module Present where

import Present.Types
import Present.Instances ()
import Present.Sexp

import Data.Monoid

-- | Make a presentation at the given cursor.
present :: Present a => Mode -> Cursor -> a -> Presentation
present mode = presentValue mode mempty
