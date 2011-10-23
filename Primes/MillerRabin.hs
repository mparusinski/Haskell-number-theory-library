{- |
Module      :  $Header$
Description :  Module to generate primes
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Primes.MillerRabin where

import qualified ModularArithmetic.PoweringAlgorithms as PA
import ModularArithmetic.Standard
import qualified Generator.RandomGenerator as RG
import Generator.Generator

import System.Random
import Data.List
import Control.Monad

witnessTest :: Integer -> Integer -> Bool
witnessTest candidate witness
    | even candidate = False
    | otherwise      = millerRabinWitnessTest candidate witness

millerRabinWitnessTest candidate witness
    = test1 || test2
    where (d, r) = removePowersOfTwo (candidate - 1)
          test1  = first `elem` [1,candidate-1]
          test2  = evenPowersLoop first 0 (r-1)
          first  = PA.fastPowerModular witness d candidate
          evenPowersLoop number counter steps
              | counter   >= steps           = False
              | newNumber == 1               = False
              | newNumber == (candidate - 1) = True
              | otherwise = evenPowersLoop newNumber (counter + 1) steps
              where newNumber = (number * number) `mod` candidate

probPrimalityTest list candidate
    = all (millerRabinWitnessTest candidate) list

millerRabinPrimalityTest :: StdGen -> Integer -> Bool
millerRabinPrimalityTest rgState candidate
    | candidate <= 1 = False
    | candidate <= 3 = True
    | candidate == 4 = False
    | otherwise      = probPrimalityTest candidateList candidate
    where randomGenerator = RG.boundRandGenerator RG.simpleRandGenerator 2 candidate 
          candidateList   = generateSetFromGenerator 20 randomGenerator rgState

getSmallestNumOfBitSize :: Int -> Integer
getSmallestNumOfBitSize bitSize
    = let num = fromIntegral bitSize :: Integer in 2 ^ num

makeIsPrimeRule rgState = Rule isPrime
    where isPrime x 
              | even x                             = Nothing
              | millerRabinPrimalityTest rgState x = Just x
              | otherwise                          = Nothing

primeGenerator :: StdGen -> Int -> Generator StdGen Integer
primeGenerator rgState bitSize
    = isPrimeRule |> randomGenerator
    where randomGenerator = RG.boundRandGenerator RG.simpleRandGenerator low up
          low             = getSmallestNumOfBitSize bitSize
          up              = getSmallestNumOfBitSize (bitSize + 1)
          isPrimeRule     = makeIsPrimeRule rgState
