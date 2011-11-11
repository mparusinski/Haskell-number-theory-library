{- |
Module      :  $Header$
Description :  Module for elliptic curves over R/I
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

-}

module EllipticCurves.ModularEllipticCurves where

import Control.Monad

import AbstractAlgebra.Rings
import AbstractAlgebra.ModularRings
import ProjectiveSpaces.ModularProjectiveSpace

-- Recall a elliptic curves over a ring/field that 
-- do not have characteristic divisible by 2 or 3
-- Y^2 * T = X^3 + A*X * T^2 + B * T^3
-- Elements within it are [X:Y:Z] satisfying the 
-- above.
-- 
data (Ring a) => ModularEllipticCurve a = MEC (ModularRing a) (ModularRing a)

evaluateFunction a b [x,y,t]
  = term1 `mod_sub` (term2 `mod_add` term3 `mod_add` term4)
  where term1 = (mod_pow y 2) `mod_mult` t
        term2 = mod_pow x 3
        term3 = a `mod_mult` x `mod_mult` (mod_pow t 2)
        term4 = b `mod_mult` (mod_pow t 3)
evaluateFunction a b _
  = error "Point has wrong dimension"

evaluateAt :: 
  (Ring p) => ModularEllipticCurve p -> ModularProjectiveSpace p -> IO (ModularRing p)
evaluateAt (MEC a b) point
  = do coordinates <- getCoordinates point
       return $ evaluateFunction a b coordinates
       
