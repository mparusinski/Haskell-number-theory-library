{- |
Module      :  $Header$
Description :  Module to compute inverse modulo something
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Inverses where

import qualified GCD as GCD
import qualified PoweringAlgorithms as PA

getInverseModulo = getInverseModulo_EE

getInverseModulo_EE :: (Integral a) => a -> a -> Maybe a
getInverseModulo_EE number modulus
    | g == 1        = Just $ a `mod` modulus
    | otherwise     = Nothing
    where (g, a, b) = GCD.extendedEuclid number modulus

getInverseModulo_FP :: (Integral a) => a -> a -> Maybe a
getInverseModulo_FP number modulus
    = PA.fastPowerAlgorithm number (modulus - 1) modulus

hasInverse = hasInverse_EE

hasInverse_EE :: (Integral a) => a -> a -> Bool
hasInverse_EE number modulus
    = ((\(a,b,c) -> a) $ GCD.extendedEuclid number modulus) == 1
