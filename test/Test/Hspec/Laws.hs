module Test.Hspec.Laws where

import Data.Group
import Data.Monoid
import Test.Hspec
import Test.QuickCheck


monoidLaws :: (Arbitrary a, Eq a, Show a, Monoid a) => a -> Spec
monoidLaws t = do
  describe "mempty" $ do
    it "is a left identity" $ property $ \x ->
      mempty <> x == x `asTypeOf` t

    it "is a right identity" $ property $ \x ->
      x <> mempty == x `asTypeOf` t

  describe "mappend" $ do
    it "is associative" $ property $ \x y z ->
      (x <> y) <> z == x <> (y <> z) `asTypeOf` t

groupLaws :: (Arbitrary a, Eq a, Show a, Group a) => a -> Spec
groupLaws t = describe "invert" $ do
    it "is a left identity" $ property $ \x ->
      x <> (invert x) == mempty `asTypeOf` t

    it "is a right identity" $ property $ \x ->
      (invert x) <> x == mempty `asTypeOf` t

    it "is associative" $ property $ \x ->
      x <> (invert x) == (invert x) <> x `asTypeOf` t

groupLaws' :: (Arbitrary a, Eq a, Show a, Group a) => a -> Spec
groupLaws' t = describe "invert" $ do
    it "is a left identity" $ property $ \x ->
      x <> (invert x) == invert mempty `asTypeOf` t

    it "is a right identity" $ property $ \x ->
      (invert x) <> x == invert mempty `asTypeOf` t

    it "is associative" $ property $ \x ->
      x <> (invert x) == (invert x) <> x `asTypeOf` t
