{- |
Module      :  $Header$
Description :  Module to handling Fields
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module AbstractAlgebra.Fields where

import AbstractAlgebra.Rings

{-
 The laws satisfied by a fields are well known. See wikipedia.
 I have added substraction, division, and exponentiation for speed
 The additional functions define useful functions
-}
class (Ring a) => Field a where 
    inv  :: a -> Maybe a
    div  :: a -> a -> Maybe a
    char :: Integer


instance Field Double where -- this is an abuse of notation
    zero    = 0
    one     = 1
    add     = (+)
    mult    = (*)
    neg     = negate
    inv x   = if x == 0 then Nothing else Maybe (1/x)
    sub     = (-)
    div x y = if y == 0 then Nothing else Maybe (1/x)
    


