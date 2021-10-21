{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

-- | Test type normalization.

module NormalizeSpec where

import Control.Monad.Trans.Reader
import Lifts
import Present
import Test.Hspec

spec :: SpecWith ()
spec = do
  it
    "Int"
    (shouldBe
       (normalizeType $(lifted [t|Int|]))
       (Right (NormalCons (TypeConstructor ''Int))))
  it
    "Maybe Int"
    (shouldBe
       (normalizeType $(lifted [t|Maybe Int|]))
       (Right
          (NormalApp
             (NormalCons (TypeConstructor ''Maybe))
             [NormalCons (TypeConstructor ''Int)])))
  it
    "IdentityT m"
    (shouldBe
       (normalizeType $(lifted [t|forall r (m :: * -> *) a. ReaderT r m a|]))
       (Right
          (NormalApp
             (NormalCons (TypeConstructor ''Maybe))
             [NormalCons (TypeConstructor ''Int)])))
