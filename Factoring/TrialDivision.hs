
module Main where

incrementLoop :: [Int]
incrementLoop = [6,4,2,4,2,4,6,2]

trialDivision :: (Integral a) => a -> [a]
trialDivision n
    | n < 0     = trialDivision (-n)
    | n == 0    = []
    | n <= 7    = return n
    | rem2 == 0 = 2 : trialDivision quot2
    | rem3 == 0 = 3 : trialDivision quot3
    | rem5 == 0 = 5 : trialDivision quot5
    | rem7 == 0 = 7 : trialDivision quot7
    | otherwise = trialDivisionLoop n 11 (floor . sqrt $ fromIntegral n) 1
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
          nextI       = (i + 1) `mod` 8

