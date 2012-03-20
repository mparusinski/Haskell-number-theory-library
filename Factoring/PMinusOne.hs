{- |
Module      :  $Header$
Description :  Module implementing p - 1 algorithm
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Factoring.PMinusOne where

import ModularArithmetic.GCD
import qualified ModularArithmetic.PoweringAlgorithms as PA
import Primes.Sieve

-- returns Nothing is no factors found
--pMinusOne :: (Integral a) => a -> a -> a -> Maybe a
pMinusOne number bound candidate
    = pMinusOneLoop number bound candidate primesUpToBound
    where primesUpToBound = eratosthenesSieve bound 


--pMinusOneLoop :: (Integral a) => a -> a -> a -> [a] -> Maybe a
pMinusOneLoop number bound candidate [] = Nothing
pMinusOneLoop number bound candidate (p:ps)
    | gcd > 1 && gcd < number = Just gcd
    | otherwise               = pMinusOneLoop number bound candidate ps
    where testNumber    = (PA.fastPower candidate highestPowerP) - 1
          highestPowerP = findHighestPowerDividing p bound
          (gcd, a, b)   = extendedEuclid testNumber number


-- this is a helper function no error checking!!!!
findHighestPowerDividing p n
    | n == 1    = 1
    | rem == 0  = p * findHighestPowerDividing p quot
    | otherwise = 1
    where (quot, rem) = divMod n p