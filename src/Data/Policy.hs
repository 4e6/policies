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
  , increase
  , decrease
  ) where

import Data.Group
import Data.Set


data Policy a
  = Forbid a
  | Permit a
  | Noop
  deriving (Eq, Show)

type Constraint a = Policy (Set a)

newtype Restrict a = Restrict { getRestrict :: Constraint a }
  deriving (Eq, Show)

newtype Relax a = Relax { getRelax :: Constraint a }
  deriving (Eq, Show)

allow :: Ord a => Constraint a
allow = Forbid []

deny :: Ord a => Constraint a
deny = Permit []

-- | Combine two constraints into more restrictive one.
increase :: Ord a => Constraint a -> Constraint a -> Constraint a
increase (Forbid xs) (Forbid ys) = Forbid $ xs `union` ys
increase (Permit xs) (Permit ys) = Permit $ xs `intersection` ys
increase (Permit ws) (Forbid bs)
  | ws == bs                     = Noop
  | otherwise                    = Permit $ ws \\ bs
increase (Forbid bs) (Permit ws)
  | ws == bs                     = Noop
  | otherwise                    = Permit $ ws \\ bs
increase c           Noop        = c
increase Noop        c           = c

-- | Combine two constraints into more relaxed one.
decrease :: Ord a => Constraint a -> Constraint a -> Constraint a
decrease (Forbid xs) (Forbid ys) = Forbid $ xs `intersection` ys
decrease (Permit xs) (Permit ys) = Permit $ xs `union` ys
decrease (Permit ws) (Forbid bs)
  | ws == bs                     = Noop
  | otherwise                    = Forbid $ bs \\ ws
decrease (Forbid bs) (Permit ws)
  | ws == bs                     = Noop
  | otherwise                    = Forbid $ bs \\ ws
decrease c           Noop        = c
decrease Noop        c           = c

-- Monoids

-- | Default restrictive strategy.
instance Ord a => Monoid (Constraint a) where
  mempty = Noop
  mappend = increase

-- | Restricting strategy - permit everything then apply restrictive policies.
instance Ord a => Monoid (Restrict a) where
  mempty = Restrict Noop
  mappend (Restrict x) (Restrict y) = Restrict $ increase x y

-- | Relaxing strategy - forbid everything then apply relaxing policies.
instance Ord a => Monoid (Relax a) where
  mempty = Relax Noop
  mappend (Relax x) (Relax y) = Relax $ decrease x y

-- Groups

instance Ord a => Group (Constraint a) where
  invert (Forbid xs) = Permit xs
  invert (Permit xs) = Forbid xs
  invert Noop        = Noop

instance Ord a => Group (Restrict a) where
  invert (Restrict (Forbid xs)) = Restrict $ Permit xs
  invert (Restrict (Permit xs)) = Restrict $ Forbid xs
  invert (Restrict Noop)        = Restrict Noop

instance Ord a => Group (Relax a) where
  invert (Relax (Forbid xs)) = Relax $ Permit xs
  invert (Relax (Permit xs)) = Relax $ Forbid xs
  invert (Relax Noop)        = Relax Noop
