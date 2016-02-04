{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Structured, lazy presentations of Haskell data types for
-- inspection while developing.

module Present
  (module Present.Types
  ,module Present.TH
  ,module Present)
  where

import Present.Types
import Present.TH

-- | Make a presentation at the given cursor.
present :: Present a => Cursor -> a -> Presentation
present = presentValue mempty

$(makePresent ''Either)
