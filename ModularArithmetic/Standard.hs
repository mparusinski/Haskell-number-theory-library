{- |
Module      :  $Header$
Description :  Module to deal with generic modular arithmetic functions
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module ModularArithmetic.Standard where

euclideanDivision = divMod

removePowersOfTwo 0 = (0, 0)
removePowersOfTwo n
    = removePowersOfTwoAccum n 0
 
removePowersOfTwoAccum n accum
    | rem == 1  = (n, accum)
    | otherwise = removePowersOfTwoAccum quot (accum + 1)
    where (quot, rem) = euclideanDivision n 2
