{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

newtype Restrict a = Restrict { getRestrict :: Constraint a }
  deriving (Eq, Show)

newtype Relax a = Relax { getRelax :: Constraint a }
  deriving (Eq, Show)

allow :: Ord a => Constraint a
allow = Forbid $ fromList []

deny :: Ord a => Constraint a
deny = Permit $ fromList []

list :: Constraint a -> [a]
list (Forbid xs) = toList xs
list (Permit xs) = toList xs

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

-- Groups
--
-- Forbid [] < Z < Permit []
-- allow     < ? < deny
-- -inf      < 0 < inf
-- narrow          wide

instance Ord a => Group (Constraint a) where
  invert (Forbid xs) = Permit xs
  invert (Permit xs) = Forbid xs

instance Ord a => Group (Relax a) where
  invert (Relax (Forbid xs)) = Relax $ Permit xs
  invert (Relax (Permit xs)) = Relax $ Forbid xs
