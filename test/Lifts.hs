{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Derive needed instances of Lift.

module Lifts where

import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax

$(deriveLiftMany [''Type, ''TyVarBndr, ''TyLit])

lifted :: Q Type -> Q Exp
lifted m = m >>= lift
