{-# LANGUAGE NoImplicitPrelude #-}

module Crypto where

import Data.Bits
import Data.List (unfoldr)
import Ourlude

-- | Calculate the gcd of two integers, and the two factors that satisfy bezout's theorem
bezout :: Integer -> Integer -> (Integer, Integer, Integer)
bezout a b | a < b =
  let (g, x, y) = bezout b a
  in (g, y, x)
bezout a 0 = (a, 1, 0)
bezout a b =
  let q = a `div` b
      r = a `mod` b
      (d, x, y) = bezout b r
   in (d, y, x - q * y)
