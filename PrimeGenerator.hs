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

module PrimeGenerator  where

import ModularArithmetic.PoweringAlgorithms
import System.Random
import Data.List
import Control.Monad

witnessTest :: Integer -> Integer -> Bool
witnessTest candidate witness
    | even candidate = False
    | otherwise      = performTest candidate witness

performTest candidate witness
    = test1 || test2
    where (d, r) = decomposeInPowerTwo (candidate - 1)
          test1  = first `elem` [1, candidate - 1]
          test2  = useHelperFunction first 0 r
          first  = fastPowerModular witness d candidate
          useHelperFunction number counter steps
              | counter   == steps           = False
              | newNumber == 1               = False
              | newNumber == (candidate - 1) = True
              | otherwise = useHelperFunction newNumber (counter + 1) steps
              where newNumber = (number * number) `mod` candidate

decomposeInPowerTwo 0 = (0, 0)
decomposeInPowerTwo n 
    = callFuncWithAccum n 0 
    where callFuncWithAccum n accum
              | odd n     = (n, accum)
              | otherwise = callFuncWithAccum (n `div` 2) (accum + 1)

probPrimalityTest list candidate
    = all (performTest candidate) list

generateList :: (RandomGen g) =>
                g -> (Integer, Integer) -> Integer -> IO [Integer]
generateList _ _ 0 = return []
generateList generator (minBound, maxBound) numOfElements
    | minBound >= maxBound = error "maxBound should be bigger"
    | otherwise            =
        do 
          let (num, newGenerator) = randomR (minBound, maxBound) generator
          tailOfList <- 
              generateList newGenerator (minBound, maxBound) (numOfElements-1)
          if num `elem` tailOfList
              then generateList newGenerator (minBound, maxBound) numOfElements
              else return (num : tailOfList)

millerRabinPrimalityTest :: Integer -> IO Bool
millerRabinPrimalityTest candidate
    | candidate <= 1 = return False
    | candidate <= 4 = return True
    | otherwise      =
        do
          let numOfElements = let n = 50 in
                  if candidate > (n * 20)
                      then n
                      else candidate `div` 20
          randomGenerator <- getStdGen
          list <- generateList randomGenerator (2, candidate - 2) numOfElements
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
  let tmpBitSize = bitSize - 1
  let lowBound   = getSmallestNumOfBitSize tmpBitSize
  let upBound    = getSmallestNumOfBitSize (tmpBitSize + 1) - 1
  randomGenerator <- getStdGen
  let getNum gen = do 
        let (num, nextGen) = randomR (lowBound, upBound) gen
        return (2 * num + 1, nextGen) 
  let isPrime    = millerRabinPrimalityTest
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

