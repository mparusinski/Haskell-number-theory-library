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

--import Primes.MillerRabin

-- main = do 
--   result <- generatePrime 2000
--   print result

import ModularArithmetic.GCD
          
simpleTest x y n 
    = 0 == (mod (x ^ 2 - y ^ 2) n)

bigTest n
    = filter func list
    where list = [(x,y) | x <- [1..(n-1)], y <- [1..(n-1)]]
          func = (\(x,y) -> simpleTest x y n)

furthermore n
    = filter func $ bigTest n
    where func = (\(x,y) -> 0 /= (mod (x-y) n) && 0 /= (mod (x+y) n))
