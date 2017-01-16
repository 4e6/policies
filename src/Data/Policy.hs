{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Policy
  ( Policy(..)
  , Restrict(..)
  , Relax(..)
  , allow
  , deny
  , list
  , increase
  , decrease
  ) where

import qualified Data.Group as G (Group, invert)
import           Data.Lattice
import           Data.Set
import           Test.QuickCheck


data Policy a
  = Forbid (Set a)
  | Permit (Set a)
  deriving (Eq, Show)

instance (Arbitrary a, Ord a) => Arbitrary (Policy a) where
  arbitrary = arbitrary >>= \case
    True  -> Forbid <$> arbitrary
    False -> Permit <$> arbitrary

newtype Restrict a = Restrict { unRestrict :: Policy a }
  deriving (Eq, Show)

instance (Arbitrary a, Ord a) => Arbitrary (Restrict a) where
  arbitrary = Restrict <$> arbitrary

newtype Relax a = Relax { unRelax :: Policy a }
  deriving (Eq, Show)

instance (Arbitrary a, Ord a) => Arbitrary (Relax a) where
  arbitrary = Relax <$> arbitrary

allow :: Ord a => Policy a
allow = Forbid []

deny :: Ord a => Policy a
deny = Permit []

list :: Policy a -> [a]
list (Forbid xs) = toList xs
list (Permit xs) = toList xs

invert :: Policy a -> Policy a
invert (Forbid a) = Permit a
invert (Permit a) = Forbid a

-- | Combine two constraints into more restrictive one.
increase :: Ord a => Policy a -> Policy a -> Policy a
increase (Forbid xs) (Forbid ys) = Forbid $ xs `union` ys
increase (Permit xs) (Permit ys) = Permit $ xs `intersection` ys
increase (Permit ws) (Forbid bs) = Permit $ ws \\ bs
increase (Forbid bs) (Permit ws) = Permit $ ws \\ bs

-- | Combine two constraints into more relaxed one.
decrease :: Ord a => Policy a -> Policy a -> Policy a
decrease (Forbid xs) (Forbid ys) = Forbid $ xs `intersection` ys
decrease (Permit xs) (Permit ys) = Permit $ xs `union` ys
decrease (Permit ws) (Forbid bs) = Forbid $ bs \\ ws
decrease (Forbid bs) (Permit ws) = Forbid $ bs \\ ws

-- Monoids
--
-- Forbid [] < Permit []
-- allow     < deny
-- 0 ..      < inf
-- narrow      wide

-- | Default restrictive strategy.
instance Ord a => Monoid (Policy a) where
  mempty = allow
  mappend = increase

-- | Restricting strategy - permit everything then apply restrictive policies.
instance Ord a => Monoid (Restrict a) where
  mempty = Restrict allow
  mappend (Restrict x) (Restrict y) = Restrict $ increase x y

-- | Relaxing strategy - forbid everything then apply relaxing policies.
instance Ord a => Monoid (Relax a) where
  mempty = Relax deny
  mappend (Relax x) (Relax y) = Relax $ decrease x y

-- Rings

-- With credit to @int-index
-- https://gist.github.com/int-index/3e08592520e5fbc7a05e090dad9a626c

instance Ord a => Num (Policy a) where
  (+) = increase
  (*) = decrease
  negate = invert

  fromInteger 0 = allow
  fromInteger 1 = deny
  fromInteger x = error $ "Invalid constraint " ++ show x

  abs = error "Abs not possible for Constraint"
  signum = error "Signum not posible for Constraint"

-- Lattice

instance Ord a => MeetSemilattice (Policy a) where
  meet = increase

instance Ord a => JoinSemilattice (Policy a) where
  join = decrease

instance Ord a => Lattice (Policy a)

instance Ord a => BoundedLattice (Policy a) where
  top = allow
  bot = deny

-- Groups

-- TODO: This way identity law doesn't hold:
--   x <> (invert x) == mempty
-- but instead other equation holds:
--   x <> (invert x) == invert mempty
-- WTF is this construction?

-- TODO: To be able to satisfy identity law we can add artificial zero element
-- in between [allow .., Z, .. deny], similar to natural numbers.
-- See branch `noop` with attempt to introduce zero Noop element
--
-- Forbid [] < Z < Permit []
-- allow     < ? < deny
-- -inf      < 0 < inf
-- narrow          wide

instance Ord a => G.Group (Policy a) where
  invert = invert

instance Ord a => G.Group (Relax a) where
  invert = Relax . invert . unRelax
  -- invert (Relax x) = Relax $ invert x
  -- invert (Relax (Forbid p)) = Relax $ Permit p
  -- invert (Relax (Permit p)) = Relax $ Forbid p
