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
import Data.Maybe
import System.Random
import IO
import System( getArgs )

import Factoring.Lenstra
import Primes.MillerRabin
import Generator.RandomGenerator
import Generator.Generator

performTrialFactoring num
    = do before <- getCPUTime
         factor <- return $! lenstraECMSmartBound num
         after  <- getCPUTime
         let diff = fromIntegral (after - before) / 10^6
         return (factor, diff)

performTrialFactoringParallel num
    = do before <- getCPUTime
         factor <- return $! lenstraECMParallelSmartBound num
         after  <- getCPUTime
         let diff = fromIntegral (after - before) / 10^6
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

ecmStandardFull :: StdGen -> Integer -> [Integer]
ecmStandardFull randomGen number
  = error "To be implemented"

factorUsingECMStandard :: Integer -> IO (Maybe Integer)
factorUsingECMStandard = computeEagerly lenstraECMSmartBound

factorUsingECMParallel :: Integer -> IO (Maybe Integer)
factorUsingECMParallel = computeEagerly lenstraECMParallelSmartBound

-- generateProductTwoPrimes bitSize
--     = do updateIORandomGenerator
--          stdGen1 <- getStdGen
--          let pg = primeGenerator stdGen1 bitSize
--          updateIORandomGenerator
--          stdGen2 <- getStdGen
--          let ([prime1, prime2], state) = runGeneratorNTimes 2 pg stdGen2
--          return (prime1 * prime2, prime1, prime2)

-- benchmarkNonParallel product
--     = bench "Non parallel" (nfIO $ performAction trialFactoring product)

-- benchmarkParallel product
--     = bench "Parallel" (nfIO $ performAction trialFactoringParallel product)
    
-- performAction action product
--     = do factor <- action product
--          return ()

-- initialPerformAction action product
--     = do factor <- action product
--          putStrLn $ "found factor "++show (fromJust factor)

main = do putStrLn "Hello, World!"

-- main = do putStrLn "Give me bit size"
--           bitSize <- liftM read getLine
--           (product, prime1, prime2) <- generateProductTwoPrimes bitSize
--           putStrLn $ show product++" = "++show prime1++" x "++show prime2
--           initialPerformAction trialFactoringParallel product
--           initialPerformAction trialFactoring product
--           defaultMain [benchmarkNonParallel product, 
--                        benchmarkParallel product]
>>>>>>> 5a923a2e6b59a71c47488b528219d275a25bc314


