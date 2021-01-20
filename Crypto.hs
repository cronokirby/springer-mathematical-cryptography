{-# LANGUAGE NoImplicitPrelude #-}

module Crypto where

import Data.Bits
import Data.List (foldl')
import Ourlude

-- | Calculate the gcd of two integers, and the two factors that satisfy bezout's theorem
bezout :: Integer -> Integer -> (Integer, Integer, Integer)
bezout a b
  | a < b =
    let (g, x, y) = bezout b a
     in (g, y, x)
bezout a 0 = (a, 1, 0)
bezout a b =
  let q = a `div` b
      r = a `mod` b
      (d, x, y) = bezout b r
   in (d, y, x - q * y)

type Modulus = Integer

normalize :: Modulus -> Integer -> Integer
normalize p x = mod (mod x p + p) p

-- | Find the multiplicative inverse of some integer, modulo some prime
--
-- This will error if this isn't possible
inverse :: Modulus -> Integer -> Integer
inverse p x =
  let (1, _, u) = bezout p x
   in normalize p u

-- | Fast exponention, modulo p
pow :: Modulus -> Integer -> Integer -> Integer
pow p g k | k < 0 = inverse p (pow p g (- k))
pow p g k = zip (bits k) squares |> filter fst |> map snd |> prod
  where
    squares :: [Integer]
    squares = iterate (\x -> x * x `mod` p) (normalize p g)

    bits :: Integer -> [Bool]
    bits 0 = []
    bits x = testBit x 0 : bits (shiftR x 1)

    prod :: [Integer] -> Integer
    prod = foldl' (\acc x -> acc * x `mod` p) 1
