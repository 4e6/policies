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

-- | Ring laws for Num
numLaws :: (Arbitrary a, Eq a, Show a, Num a) => a -> Spec
numLaws t = do
  describe "abelian group under addititon" $ do
    it "is associative" $ property $ \x y z ->
      (x + y) + z == x + (y + z) `asTypeOf` t

    it "is commutative" $ property $ \x y ->
      x + y == y + x `asTypeOf` t

    it "is additive identity" $ property $ \x ->
      x + (fromInteger 0) == x `asTypeOf` t

    it "is additive inverse" $ property $ \x ->
      x + (negate x) == fromInteger 0 `asTypeOf` t

  describe "monoid under multiplication" $ do
    it "is associative" $ property $ \x y z ->
      (x * y) * z == x * (y * z) `asTypeOf` t

    it "is multiplicative identity" $ property $ \x ->
      x * (fromInteger 1) == (fromInteger 1) * x `asTypeOf` t

  describe "multiplication is distributive with respect to addition" $ do
    it "has left distributivity" $ property $ \x y z ->
      x * (y + z) == (x * y) + (x * z) `asTypeOf` t

    it "has right distributivity" $ property $ \x y z ->
      (y + z) * x == (y * x) + (z * x) `asTypeOf` t

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
