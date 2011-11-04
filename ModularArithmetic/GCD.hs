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
extendedEuclid 0 other = (other, 0, 1)
extendedEuclid other 0 = (other, 1, 0)
extendedEuclid number other
    | remainder == 0 = (other, 0, quotient)
    | remainder == 1 = (1, 1, - quotient)
    | otherwise      = (gcd, b, a - b * quotient)
    where (quotient, remainder) = euclideanDivision number other
          (gcd, a, b)           = extendedEuclid other remainder


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

