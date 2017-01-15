{-# LANGUAGE NoImplicitPrelude #-}

module Data.Semilattice where

import Prelude hiding ((^))


class MeetSemilattice a where
  meet :: a -> a -> a

  (^) :: a -> a -> a
  (^) = meet

class JoinSemilattice a where
  join :: a -> a -> a

  v :: a -> a -> a
  v = join


prop_MeetSemilatticeAssociative :: (Eq a, MeetSemilattice a) => a -> a -> a -> Bool
prop_MeetSemilatticeAssociative a b c = a ^ (b ^ c) == (a ^ b) ^ c

prop_MeetSemilatticeCommutative :: (Eq a, MeetSemilattice a) => a -> a -> Bool
prop_MeetSemilatticeCommutative a b = a ^ b == b ^ a

prop_MeetSemilatticeIdempotent :: (Eq a, MeetSemilattice a) => a -> Bool
prop_MeetSemilatticeIdempotent a = a ^ a == a

prop_JoinSemilatticeAssociative :: (Eq a, JoinSemilattice a) => a -> a -> a -> Bool
prop_JoinSemilatticeAssociative a b c = a `v` (b `v` c) == (a `v` b) `v` c

prop_JoinSemilatticeCommutative :: (Eq a, JoinSemilattice a) => a -> a -> Bool
prop_JoinSemilatticeCommutative a b = a `v` b == b `v` a

prop_JoinSemilatticeIdempotent :: (Eq a, JoinSemilattice a) => a -> Bool
prop_JoinSemilatticeIdempotent a = a `v` a == a
