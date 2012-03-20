{- |
Module      :  $Header$
Description :  Module to control random number generation
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Generator.RandomGenerator where

import Generator.Generator

import System.Random
import Control.Monad.State

simpleRandGenerator :: (Integral a, Random a) => Generator StdGen a
simpleRandGenerator 
    = Generator $ 
      do randomGenerator <- get 
         let (output, nextGenerator) = random randomGenerator 
         put nextGenerator 
         return output

boundRandGenerator :: (Integral a) => 
                      Generator StdGen a -> a -> a -> Generator StdGen a 
boundRandGenerator generator lowBound upBound
    = boundRule |> generator
    where boundRule = generateBoundedRule lowBound upBound

updateIORandomGenerator :: IO ()
updateIORandomGenerator 
    = do
  actualRandomNumber <- randomIO
  let generator = mkStdGen actualRandomNumber
  setStdGen generator

updateIORandomGeneratorWithNum :: Int -> IO ()
updateIORandomGeneratorWithNum number
    = do 
  let generator = mkStdGen number
  setStdGen generator

