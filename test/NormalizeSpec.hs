{-# LANGUAGE TemplateHaskell #-}

-- | Test type normalization.

module NormalizeSpec where

import Language.Haskell.TH.Syntax
import Present
import Test.Hspec

spec :: SpecWith ()
spec = do
  it
    "Int"
    (shouldBe
       (normalizeType (ConT ''Int))
       (Right (NormalCons (TypeConstructor ''Int))))
  it
    "Maybe Int"
    (shouldBe
       (normalizeType (AppT (ConT ''Maybe) (ConT ''Int)))
       (Right
          (NormalApp
             (NormalCons (TypeConstructor ''Maybe))
             [NormalCons (TypeConstructor ''Int)])))
