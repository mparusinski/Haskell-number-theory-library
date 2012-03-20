{- |
Module      :  $Header$
Description :  Module to compute GCDs
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module ModularArithmetic.GCD where

import ModularArithmetic.Standard

import Data.Bits

-- euclideanDivision should refer to an optimised 
-- version of the euclidean division
-- extendedEuclid 0 other = (other, 0, 1)
-- extendedEuclid other 0 = (other, 1, 0)
-- extendedEuclid number other
--     | remainder == 0 = (other, 0, quotient)
--     | remainder == 1 = (1, 1, - quotient)
--     | otherwise      = (gcd, b, a - b * quotient)
--     where (quotient, remainder) = euclideanDivision number other
--           (gcd, a, b)           = extendedEuclid other remainder

extendedEuclid :: (Integral a) => a -> a -> (a, a, a)
extendedEuclid a b -- using iterative approach
    | d < 1     = (-d, -u, -v)
    | otherwise = (d, u, v) 
    where (d, u, v) = extendedEuclidAccum (a, 1, 0) (b, 0, 1) 
          extendedEuclidAccum (prevRem, prevU, prevV) (currRem, currU, currV)
              | currRem == 0    = (prevRem, prevU, prevV)
              | otherwise       = nextTriplet `seq` recurse 
              where nextTriplet = (r, prevU - q * currU, prevV - q * currV)  
                    (q,r)       = divMod prevRem currRem
                    recurse     = extendedEuclidAccum (currRem, currU, currV) nextTriplet

-- PRE:  All of [a] are not divisible by N
-- POST: Outputs the list of inverses of a factor of N
parallelInverse :: (Integral a) => [a] -> a -> Either [a] a
parallelInverse as n
    = if d > 1 
      then Right d 
      else Left . snd $ loop productsComputed as 1
    where (d,u,v)          = extendedEuclid (last $ productsComputed) n
          productsComputed = productsMod as n
          loop [c] [a] prevC = (u * a `mod` n, [u * prevC `mod` n])
          loop (c:cs) (a:as) prevC
              = (currentU * a `mod` n, (currentU * prevC `mod` n) : inverses)
              where (currentU, inverses) = loop cs as c

productsMod (a:as) n
    = productsModAccum as a n
    where productsModAccum [] acc _ = [acc]
          productsModAccum (a:as) acc n
              = acc : productsModAccum as (acc * a `mod` n) n


computeGCD a b = firstOf3 $ extendedEuclid a b
  where firstOf3 (x,y,z) = x

gcdOfList []  = 1 -- 1 divides everything
gcdOfList [x] = x 
gcdOfList (x:xs)
  = computeGCD x (gcdOfList xs)

binaryGCD 0 other = other
binaryGCD other 0 = other
binaryGCD a b 
    = binaryGCDEvenLoop a b 0

binaryGCDEvenLoop a b k
    | odd a || odd b = binaryGCDOneOddLoop a b k
    | otherwise      = binaryGCDEvenLoop (shift a (-1)) (shift b (-1)) (k+1)

binaryGCDOneOddLoop a b k
    | odd a && even b = binaryGCDOneOddLoop a (shift b (-1)) k
    | even a && odd b = binaryGCDOneOddLoop (shift a (-1)) b k
    | a >= b          = binaryGCDSubLoop (shift (a-b) (-1)) b k
    | otherwise       = binaryGCDSubLoop a (shift (b-a) (-1)) k

binaryGCDSubLoop a b k
    | a == b || a == 0 = shift b k
    | otherwise        = binaryGCDOneOddLoop a b k

