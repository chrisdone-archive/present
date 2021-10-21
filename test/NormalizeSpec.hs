{-# LANGUAGE TemplateHaskell #-}

-- | Test type normalization.

module NormalizeSpec where

import Present
import Test.Hspec
import Lifts

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
