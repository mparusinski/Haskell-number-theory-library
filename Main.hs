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

import Factoring.TrialDivision
import ModularArithmetic.GCD

n = 35

tuples :: [(Integer, Integer)]
tuples
    = do
  let list = [1..(n-1)]
  x <- list
  y <- list
  return (x,y)

filteringFunction :: (Integer, Integer) -> Bool
filteringFunction (x,y)
    = gcd == 1
    where discriminant = (4 * x ^ 3 + 27 * y ^ 2) `mod` n
          (gcd, a, b)  = extendedEuclid discriminant n

candidates = 
    filter filteringFunction tuples

main = do 
  print candidates
