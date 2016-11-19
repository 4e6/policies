{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}

module Data.Policy
  ( Policy(..)
  , Constraint
  , Restrict(..)
  , Relax(..)
  , allow
  , deny
  , list
  , increase
  , decrease
  ) where

import Data.Group
import Data.Set


data Policy a
  = Forbid a
  | Permit a
  deriving (Eq, Show)

type Constraint a = Policy (Set a)

newtype Restrict a = Restrict { unRestrict :: Constraint a }
  deriving (Eq, Show)

newtype Relax a = Relax { unRelax :: Constraint a }
  deriving (Eq, Show)

allow :: Ord a => Constraint a
allow = Forbid []

deny :: Ord a => Constraint a
deny = Permit []

list :: Constraint a -> [a]
list (Forbid xs) = toList xs
list (Permit xs) = toList xs

inversion :: Policy a -> Policy a
inversion (Forbid a) = Permit a
inversion (Permit a) = Forbid a

-- | Combine two constraints into more restrictive one.
increase :: Ord a => Constraint a -> Constraint a -> Constraint a
increase (Forbid xs) (Forbid ys) = Forbid $ xs `union` ys
increase (Permit xs) (Permit ys) = Permit $ xs `intersection` ys
increase (Permit ws) (Forbid bs) = Permit $ ws \\ bs
increase (Forbid bs) (Permit ws) = Permit $ ws \\ bs

-- | Combine two constraints into more relaxed one.
decrease :: Ord a => Constraint a -> Constraint a -> Constraint a
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
instance Ord a => Monoid (Constraint a) where
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

instance Ord a => Num (Constraint a) where
  (+) = increase
  (*) = decrease
  negate = inversion

  fromInteger 0 = allow
  fromInteger 1 = deny
  fromInteger x = error $ "Invalid constraint " ++ show x

  abs = error "Abs not possible for Constraint"
  signum = error "Signum not posible for Constraint"

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

instance Ord a => Group (Constraint a) where
  invert = inversion

instance Ord a => Group (Relax a) where
  invert = Relax . inversion . unRelax
  -- invert (Relax x) = Relax $ inversion x
  -- invert (Relax (Forbid p)) = Relax $ Permit p
  -- invert (Relax (Permit p)) = Relax $ Forbid p
