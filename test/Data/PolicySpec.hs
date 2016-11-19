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
  describe "Monoid Policy" $ do monoidLaws p
  describe "Monoid Restrict" $ do monoidLaws p
  describe "Monoid Relax" $ do monoidLaws p
  describe "Num Policy" $ do numLaws p
  describe "Group Policy" $ do groupLaws p
  describe "Group Relax" $ do groupLaws (Relax p)
  describe "Group* Policy" $ do groupLaws' p
