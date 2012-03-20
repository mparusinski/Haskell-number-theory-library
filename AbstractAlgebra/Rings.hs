{- |
Module      :  $Header$
Description :  Module handling rings
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module AbstractAlgebra.Rings where

class (Eq a) => Ring a where
    zero :: a
    one  :: a
    add  :: a -> a -> a
    mult :: a -> a -> a
    neg  :: a -> a
    sub  :: a -> a -> a


pow :: (Ring a, Integral b) => a -> b -> a
pow _ 0        = one
pow mantissa 1 = mantissa
pow mantissa exponent
    | rem == 0  = mult term term
    | otherwise = mult (mult term term) mantissa
    where term        = pow mantissa half
          (half, rem) = divMod exponent 2


instance Ring Integer where
    zero = 0
    one  = 1
    add  = (+)
    mult = (*)
    neg  = negate
    sub  = (-)
