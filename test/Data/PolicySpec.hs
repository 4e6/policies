{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.PolicySpec where

import Data.Policy
import Data.Proxy
import Test.Hspec
import Test.Hspec.Laws


p :: Policy Int
p = allow

proxy :: Proxy (Policy Int)
proxy = Proxy

spec :: Spec
spec = do
  describe "Monoid Policy"          $ monoidLaws p
  describe "Monoid Restrict"        $ monoidLaws p
  describe "Monoid Relax"           $ monoidLaws p
  describe "Num Policy"             $ numLaws p
  describe "Group Policy"           $ groupLaws p
  describe "Group Relax"            $ groupLaws (Relax p)
  describe "Group* Policy"          $ groupLaws' p
  describe "Lattice Policy"         $ latticeLaws proxy
  describe "Bounded Lattice Policy" $ boundedLatticeLaws proxy
