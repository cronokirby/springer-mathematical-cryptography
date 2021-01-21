{-# LANGUAGE NoImplicitPrelude #-}

module Crypto where

import Data.Bits
import Data.List (find, foldl')
import qualified Data.Map.Strict as Map
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

-- | Find the (floor of) the square root of an integer
squareRoot :: Integer -> Integer
squareRoot n
  | n < 0 = error "negative number"
  | n == 0 = 0
  | n > 0 = go n
  where
    go a =
      let b = div (a + div n a) 2
       in if a > b then go b else a

-- | In some modular context, find an x such that g^x = h
babyStepGiantStep :: Modulus -> Integer -> Integer -> Maybe Integer
babyStepGiantStep p g h =
  let n = 1 + squareRoot p
      babySteps = iterate (\x -> x * g `mod` p) 1 |> (`zip` [0 .. n]) |> Map.fromList
      g' = pow p (inverse p g) n
      giantSteps = iterate (\x -> x * g' `mod` p) h |> take (fromIntegral n)
   in do
        (j, x) <- find (snd >>> (`Map.member` babySteps)) (zip [0 ..] giantSteps)
        i <- Map.lookup x babySteps
        return (i + j * n)