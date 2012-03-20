{- |
Module      :  $Header$
Description :  Module implementing trial division algorithm
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Factoring.TrialDivision where

incrementLoop :: [Int]
incrementLoop = [6,4,2,4,2,4,6,2]

trialDivision :: (Integral a) => a -> [a]
trialDivision n
    | n < 0     = trialDivision (-n)
    | n == 0    = []
    | n == 2    = return 2
    | n == 3    = return 3
    | n == 5    = return 5
    | n == 7    = return 7
    | rem2 == 0 = 2 : trialDivision quot2
    | rem3 == 0 = 3 : trialDivision quot3
    | rem5 == 0 = 5 : trialDivision quot5
    | rem7 == 0 = 7 : trialDivision quot7
    | otherwise = trialDivisionLoop n 11 (floor . sqrt $ fromIntegral n) 2
    where (quot2, rem2) = divMod n 2
          (quot3, rem3) = divMod n 3
          (quot5, rem5) = divMod n 5
          (quot7, rem7) = divMod n 7

trialDivisionLoop :: (Integral a) => a -> a -> a -> Int -> [a]
trialDivisionLoop 1 _ _ _ = []
trialDivisionLoop n candidate bound i
    | candidate > bound  = [n]
    | rem == 0           = candidate : trialDivisionLoop quot candidate bound i
    | otherwise          = trialDivisionLoop n (candidate + incr) bound nextI
    where (quot, rem) = divMod n candidate
          incr        = fromIntegral (incrementLoop !! i)
          nextI       = (i + 1) `mod` length incrementLoop

cycleCandidates candidate i
  = candidate : cycleCandidates nextCandidate nextI
  where nextI    = (i + 1) `mod` length incrementLoop
        nextCandidate = candidate + (fromIntegral $ incrementLoop !! i)