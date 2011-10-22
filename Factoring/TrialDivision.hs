
module TrialDivision where

trialDivision :: (Integral a) => a -> [a]
trialDivision 0 = []
trialDivision 1 = [1]
trialDivision 2 = [2]
trialDivision 3 = [3]
trialDivision n
    | n <= 0    = trialDivision (-n)
    | rem == 0  = 2 : trialDivision quot
    | otherwise = trialDivisionLoop n 3
    where (quot, rem) = divMod n 2

trialDivisionLoop 1 _ = []
trialDivisionLoop n candidate
    | rem == 0           = candidate : trialDivisionLoop quot candidate
    | candidate ^ 2 <= n = trialDivisionLoop n (candidate + 2)
    | otherwise          = [n]
    where (quot, rem) = divMod n candidate
