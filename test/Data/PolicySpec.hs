{-# LANGUAGE TemplateHaskell #-}

module Data.PolicySpec where

import Data.DeriveTH
import Data.Policy
import Test.Hspec
import Test.Hspec.Laws
import Test.QuickCheck


derive makeArbitrary ''Policy

--derive makeArbitrary ''Relax
instance (Arbitrary a, Ord a) => Arbitrary (Relax a) where
  arbitrary = do
    x <- arbitrary
    return $ Relax x

instance (Arbitrary a, Ord a) => Arbitrary (Restrict a) where
  arbitrary = do
    x <- arbitrary
    return $ Restrict x

p :: Constraint Int
p = allow

spec :: Spec
spec = do
  describe "Policy" $ do
    monoidLaws p
    monoidLaws $ Restrict p
    monoidLaws $ Relax p
    groupLaws p
