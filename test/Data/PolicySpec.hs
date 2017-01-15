{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.PolicySpec where

import Data.DeriveTH
import Data.Policy
import Data.Proxy
import Test.Hspec
import Test.Hspec.Laws
import Test.QuickCheck


derive makeArbitrary ''Policy

instance (Arbitrary a, Ord a) => Arbitrary (Relax a) where
  arbitrary = Relax <$> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Restrict a) where
  arbitrary = Restrict <$> arbitrary

p :: Constraint Int
p = allow

proxy :: Proxy (Constraint Int)
proxy = Proxy

spec :: Spec
spec = do
  describe "Monoid Policy" $ monoidLaws p
  describe "Monoid Restrict" $ monoidLaws p
  describe "Monoid Relax" $ monoidLaws p
  describe "Num Policy" $ numLaws p
  describe "Group Policy" $ groupLaws p
  describe "Group Relax" $ groupLaws (Relax p)
  describe "Group* Policy" $ groupLaws' p
  describe "Lattice Policy" $ latticeLaws proxy
  describe "Bounded Lattice Policy" $ boundedLatticeLaws proxy
