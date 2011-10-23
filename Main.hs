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

import Primes.MillerRabin

main = do 
  updateRandomGeneratorWithNum 3
  result <- getRandomPrime 2000
  print result
          