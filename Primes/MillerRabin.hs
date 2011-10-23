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

--generateList :: (RandomGen g) =>
--                g -> (Integer, Integer) -> Integer -> IO [Integer]
generateList generator candidate numOfElements
    | candidate <= 25 = return [2..candidate-1]
    | otherwise       = generateListLoop generator (minBound, maxBound) numOfElements
    where minBound = 2
          maxBound = candidate - 2

generateListLoop _ _ 0 = return []
generateListLoop generator (minBound, maxBound) numOfElements
    = do 
  let (num, newGen) = randomR (minBound, maxBound) generator
  tailOfList <- generateListLoop newGen (minBound, maxBound) (numOfElements-1)
  if num `elem` tailOfList
      then generateListLoop newGen (minBound, maxBound) numOfElements
      else return (num : tailOfList)

millerRabinPrimalityTest :: Integer -> IO Bool
millerRabinPrimalityTest candidate
    | candidate <= 1 = return False
    | candidate <= 3 = return True
    | candidate == 4 = return False
    | otherwise      =
        do
          let numOfElements = 20
          randomGenerator <- getStdGen
          list <- generateList randomGenerator candidate numOfElements
          return $ probPrimalityTest list candidate

getSmallestNumOfBitSize :: Int -> Integer
getSmallestNumOfBitSize bitSize
    = let num = fromIntegral bitSize :: Integer in 2 ^ num

performActionUntil :: (a -> IO (b,a)) -> a -> (b -> IO Bool) -> IO b
performActionUntil action inputs condition
    = do 
  (result, newInputs) <- action inputs
  conditional <- condition result
  if conditional
      then return result
      else performActionUntil action newInputs condition

getRandomPrime :: Int -> IO Integer
getRandomPrime bitSize 
    = do 
  let lowBound   = getSmallestNumOfBitSize bitSize
  let upBound    = getSmallestNumOfBitSize (bitSize + 1) - 1
  randomGenerator <- getStdGen
  let getNum gen = do 
        let (num, nextGen) = randomR (lowBound, upBound) gen
        return (num, nextGen)
  let isPrime x  = do 
        let b1 = odd x 
        if b1 then millerRabinPrimalityTest x else return False
  performActionUntil getNum randomGenerator isPrime

updateRandomGenerator :: IO ()
updateRandomGenerator 
    = do
  actualRandomNumber <- randomIO
  let generator = mkStdGen actualRandomNumber
  setStdGen generator

updateRandomGeneratorWithNum :: Int -> IO ()
updateRandomGeneratorWithNum number
    = do 
  let generator = mkStdGen number
  setStdGen generator

