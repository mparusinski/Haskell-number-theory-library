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

import AbstractAlgebra.ModularRings
import AbstractAlgebra.Rings
import ModularArithmetic.Standard
import qualified Generator.RandomGenerator as RG
import qualified Generator.MonotonicGenerator as MG
import Generator.Generator

import System.Random
import Data.List
import Control.Monad

millerRabinWitnessTest :: Integer -> Integer -> Bool
millerRabinWitnessTest candidate witness
    = test1 || test2
    where (d, r) = removePowersOfTwo (candidate - 1)
          modWit = embed witness (flip mod candidate)
          test1  = (representant first) `elem` [1,candidate - 1]
          test2  = evenPowersLoop first 0 (r-1)
          first  = mod_pow modWit d
          evenPowersLoop number counter steps
              | counter   >= steps           = False
              | repre     == 1               = False
              | repre     == (candidate - 1) = True
              | otherwise = evenPowersLoop newNumber (counter + 1) steps
              where newNumber = mod_mult number number
                    repre     = representant newNumber

doAllWitnessTests list candidate
    = all (millerRabinWitnessTest candidate) list

-- using the fact that small candidates speeds up the algorithm very quickly
millerRabinPrimalityTest :: StdGen -> Integer -> Bool
millerRabinPrimalityTest rgState candidate
    | candidate <= 1 = False
    | candidate <= 3 = True
    | candidate == 4 = False
    | otherwise      = doAllWitnessTests candidateList candidate
    where randomGen     = RG.boundRandGenerator RG.simpleRandGenerator 1 upBound 
          numOfElems    = 20
          upBound       = (2 ^ 32) `div` (numOfElems + 1)
          numberGen     = MG.createMonotonicGenerator MG.Positive randomGen
          candidateList = generateListFromGenerator numOfElems numberGen (rgState,1)

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
    = isPrimeRule |> (onlyOddRule |> randomGenerator)
    where randomGenerator = RG.boundRandGenerator RG.simpleRandGenerator low up
          onlyOddRule     = Rule (\x -> Just $ 2 * x + 1)
          low             = getSmallestNumOfBitSize (bitSize - 1)
          up              = 2 * low
          isPrimeRule     = makeIsPrimeRule rgState

generatePrime :: Int -> IO Integer
generatePrime bitSize
    = do RG.updateIORandomGenerator
         stdGen1 <- getStdGen
         let pg = primeGenerator stdGen1 bitSize
         RG.updateIORandomGenerator
         stdGen2 <- getStdGen
         let (prime, stdGen3) = generate pg stdGen2
         return prime