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
import System.Time
import System.Random

import Factoring.Lenstra
import Primes.MillerRabin
import Generator.RandomGenerator
import Generator.Generator

getTimeMS 
    = do (TOD s p) <- getClockTime
         return $ fromIntegral (s * 1000 + p `div` 10^6)

performTrialFactoring num
    = do before <- getTimeMS
         factor <- return $! lenstraECMSmartBound num
         after  <- getTimeMS
         let diff = after - before
         return (factor, diff)

performTrialFactoringParallel num
    = do before <- getTimeMS
         factor <- return $! lenstraECMParallelSmartBound num
         after  <- getTimeMS
         let diff = after - before
         return (factor, diff)

generateProductTwoPrimes bitSize
    = do updateIORandomGenerator
         stdGen1 <- getStdGen
         let pg = primeGenerator stdGen1 bitSize
         updateIORandomGenerator
         stdGen2 <- getStdGen
         let ([prime1, prime2], state) = runGeneratorNTimes 2 pg stdGen2
         return (prime1 * prime2, prime1, prime2)

chosenBitSize = 30 -- bits

main = do (product, prime1, prime2) <- generateProductTwoPrimes chosenBitSize
          putStrLn $ show product++" = "++show prime1++" x "++show prime2
          (factor, diffTime) <- performTrialFactoring product
          putStrLn $ show factor++" divides "++show product
          putStrLn $ "Factor found in "++show diffTime++" sec (not parallel)"
          (factor2, diffTime2) <- performTrialFactoringParallel product
          putStrLn $ show factor++" divides "++show product
          putStrLn $ "Factor found in "++show diffTime++" sec (parallel)"


