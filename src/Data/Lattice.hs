{-# LANGUAGE NoImplicitPrelude #-}

module Data.Lattice where

import Prelude hiding ((^))

class MeetSemilattice a where
  meet :: a -> a -> a

  (^) :: a -> a -> a
  (^) = meet

class JoinSemilattice a where
  join :: a -> a -> a

  v :: a -> a -> a
  v = join

class (MeetSemilattice a, JoinSemilattice a) => Lattice a where

class Lattice a => BoundedLattice a where
  top :: a
  bot :: a


prop_MeetSemilatticeAssociative :: (Eq a, MeetSemilattice a) => a -> a -> a -> Bool
prop_MeetSemilatticeAssociative a b c = a ^ (b ^ c) == (a ^ b) ^ c

prop_MeetSemilatticeCommutative :: (Eq a, MeetSemilattice a) => a -> a -> Bool
prop_MeetSemilatticeCommutative a b = a ^ b == b ^ a

prop_MeetSemilatticeIdempotent :: (Eq a, MeetSemilattice a) => a -> Bool
prop_MeetSemilatticeIdempotent a = a ^ a == a

prop_JoinSemilatticeCommutative :: (Eq a, JoinSemilattice a) => a -> a -> Bool
prop_JoinSemilatticeCommutative a b = a `v` b == b `v` a

prop_JoinSemilatticeAssociative :: (Eq a, JoinSemilattice a) => a -> a -> a -> Bool
prop_JoinSemilatticeAssociative a b c = a `v` (b `v` c) == (a `v` b) `v` c

prop_JoinSemilatticeIdempotent :: (Eq a, JoinSemilattice a) => a -> Bool
prop_JoinSemilatticeIdempotent a = a `v` a == a

prop_MeetLatticeAbsorbtion :: (Eq a, Lattice a) => a -> a -> Bool
prop_MeetLatticeAbsorbtion a b = a `v` (a ^ b) == a

prop_JoinLatticeAbsorbtion :: (Eq a, Lattice a) => a -> a -> Bool
prop_JoinLatticeAbsorbtion a b = a ^ (a `v` b) == a

prop_MeetBoundedLatticeIdentity :: (Eq a, BoundedLattice a) => a -> Bool
prop_MeetBoundedLatticeIdentity a = a ^ top == a

prop_JoinBoundedLatticeIdentity :: (Eq a, BoundedLattice a) => a -> Bool
prop_JoinBoundedLatticeIdentity a = a `v` bot == a
