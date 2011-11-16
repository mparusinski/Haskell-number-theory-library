{- |
Module      :  $Header$
Description :  Simple Main file to test stuff
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Main where

import Control.Monad
import System.CPUTime
import System.Random

import Factoring.Lenstra
import Primes.MillerRabin
import Generator.RandomGenerator
import Generator.Generator

performTrialFactoring num
    = do before <- getCPUTime
         factor <- lenstraECMSmartBound num
         after  <- getCPUTime
         let resolution = fromIntegral cpuTimePrecision :: Double
         diffTime <- return $! fromIntegral (after - before) / resolution
         return (factor, diffTime)

generateProductTwoPrimes bitSize
    = do updateIORandomGenerator
         stdGen1 <- getStdGen
         let pg = primeGenerator stdGen1 bitSize
         updateIORandomGenerator
         stdGen2 <- getStdGen
         let ([prime1, prime2], state) = runGeneratorNTimes 2 pg stdGen2
         return (prime1 * prime2, prime1, prime2)

chosenBitSize = 20 -- bits

main = do (product, prime1, prime2) <- generateProductTwoPrimes chosenBitSize
          putStrLn $ show product ++ " = " ++ show prime1 ++ " x " ++ show prime2
          (factor, diffTime) <- performTrialFactoring product
          putStrLn $ show factor ++ " divides " ++ show product
          putStrLn $ "Factor found in " ++ show diffTime ++ " sec"


