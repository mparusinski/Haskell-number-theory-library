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
import System.IO
import Data.Maybe

import Factoring.Lenstra
import Primes.Sieve

listOfNumbers = [2..1000]

performTrialFactoring num
    = do before <- getCPUTime
         factor <- lenstraECMSmartBound num
         after  <- getCPUTime
         let resolution = fromIntegral cpuTimePrecision :: Double
         let diffTime = fromIntegral (after - before) / resolution
         return (num, factor, diffTime)

printEntry (num, factor, diffTime)
    = show num ++ "\t" ++ maybe "0" show factor ++ "\t" ++ show diffTime

-- main = do results <- mapM performTrialFactoring listOfNumbers
--           handle <- openFile "results.txt" WriteMode
--           let stringResults = map printEntry results
--           mapM_ (hPutStrLn handle) stringResults
--           hClose handle

main = lenstraECMSmartBound (7000000001 * 7000000001) >>= print

-- main = do print $ last $ eratosthenesSieve 8000000

-- main = do list <- eratosthenesSieve_io 1000000
--           print $ last $ list