module Data.Policy
  ( Policy(..)
  , Restrict(..)
  , Relax(..)
  , allow
  , deny
  , list
  , restrict
  , relax
  ) where

import Data.Group
import Data.List


data Policy a
  = Blacklist [a]
  | Whitelist [a]
  deriving (Eq, Show)

newtype Restrict a = Restrict { getRestrict :: Policy a }
  deriving (Eq, Show)

newtype Relax a = Relax { getRelax :: Policy a }
  deriving (Eq, Show)

allow :: Policy a
allow = Blacklist []

deny :: Policy a
deny = Whitelist []

list :: Policy a -> [a]
list (Blacklist xs) = xs
list (Whitelist xs) = xs

restrict :: Eq a => Policy a -> Policy a -> Policy a
restrict (Blacklist xs) (Blacklist ys) = Blacklist $ xs `union` ys
restrict (Whitelist xs) (Whitelist ys) = Whitelist $ xs `intersect` ys
restrict (Whitelist ws) (Blacklist bs) = Whitelist $ ws \\ bs
restrict (Blacklist bs) (Whitelist ws) = Whitelist $ ws \\ bs

relax :: Eq a => Policy a -> Policy a -> Policy a
relax (Blacklist xs) (Blacklist ys) = Blacklist $ xs \\ ys
relax (Whitelist xs) (Whitelist ys) = Whitelist $ xs `union` ys
relax (Whitelist ws) (Blacklist bs) = Blacklist $ bs \\ ws
relax (Blacklist bs) (Whitelist ws) = Blacklist $ bs \\ ws

-- Monoids

-- | Default restrictive strategy.
instance Eq a => Monoid (Policy a) where
  mempty = allow
  mappend = restrict

-- | Restrict strategy - allow everything then apply restrictive policies.
instance Eq a => Monoid (Restrict a) where
  mempty = Restrict allow
  mappend (Restrict x) (Restrict y) = Restrict $ restrict x y

-- | Relax strategy - deny everything then apply relaxing policies.
instance Eq a => Monoid (Relax a) where
  mempty = Relax deny
  mappend (Relax x) (Relax y) = Relax $ relax x y

-- Groups

instance Eq a => Group (Policy a) where
  invert (Blacklist xs) = Whitelist xs
  invert (Whitelist xs) = Blacklist xs
