{- |
Module      :  $Header$
Description :  Module to compute powers
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module ModularArithmetic.PoweringAlgorithms where

import ModularArithmetic.Standard

type Modifier a = (a -> a)

simplePower a b = simplePowerGeneric id a b
simplePowerModular a b modulus 
    = simplePowerGeneric (flip mod modulus) a b

simplePowerGeneric :: (Integral a) => Modifier a -> a -> a -> a
simplePowerGeneric modif mantissa exponent
    = modif (mantissa ^ exponent) 


fastPower a b = fastPowerGeneric id a b
fastPowerModular a b modulus
   = fastPowerGeneric (flip mod modulus) a b

fastPowerGeneric :: Modifier Integer -> Integer -> Integer -> Integer
fastPowerGeneric _ _ 0            = 1
fastPowerGeneric modif mantissa 1 = modif mantissa
fastPowerGeneric modif mantissa exponent
    | rem == 0  = modif (term * term)
    | otherwise = modif (term * term * mantissa)
    where term        = fastPowerGeneric modif mantissa half
          (half, rem) = euclideanDivision exponent 2
