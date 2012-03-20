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
import Data.Time
--import Criterion.Main

import Factoring.Lenstra
import Factoring.TrialDivision
import Primes.MillerRabin
import Generator.RandomGenerator
import Generator.Generator

ecmGenericFull method number
  = do updateIORandomGenerator
       stdGen <- getStdGen
       let isPrime = millerRabinPrimalityTest stdGen number
       if isPrime 
         then return [number]
         else do maybeFactor <- return $! method number
                 if isNothing maybeFactor 
                   then return [number]
                   else do leftPart <- ecmStandardFull $ fromJust maybeFactor
                           rightPart <- ecmStandardFull $ number `div` (fromJust maybeFactor)
                           return (leftPart ++ rightPart)

ecmStandardFull = ecmGenericFull lenstraECMSmartBound
ecmParallelFull = ecmGenericFull lenstraECMParallelSmartBound

trialDivisionFull num = return $! trialDivision num

generateSemiPrime bitSize
     = do updateIORandomGenerator
          stdGen1 <- getStdGen
          let pg = primeGenerator stdGen1 bitSize
          updateIORandomGenerator
          stdGen2 <- getStdGen
          let ([prime1, prime2], state) = runGeneratorNTimes 2 pg stdGen2
          return (prime1 * prime2, prime1, prime2)

-- action should always give the same output
runActionNTimes action input times
  = liftM head $ mapM action $ take times $ repeat input

times = 5

divisionRun method number methodString
  = do putStr $ "Using " ++ methodString ++ " ..."
       hFlush stdout
       start <- getCurrentTime
       factors <- runActionNTimes method number times -- it should be eager
       end <- getCurrentTime
       putStrLn $ " found factors " ++ show factors
       let timeTaken = show ((diffUTCTime end start) / (fromIntegral times))
       putStrLn $ "It took " ++ timeTaken ++ "\n"

experimentRun bitSize 
  = do putStrLn "===================================================="
       putStrLn $ "Running experiment for bit size " ++ show bitSize
       (product, first, second) <- generateSemiPrime bitSize
       putStrLn $ show product ++ " = " ++ show first ++ " x " ++ show second ++ "\n"
       divisionRun trialDivisionFull product "trial division"
       divisionRun ecmStandardFull product "ECM Standard"
       divisionRun ecmParallelFull product "ECM Parallel"
       putStrLn " "

main = do let bitSizes = [1..20]
          mapM_ experimentRun bitSizes


