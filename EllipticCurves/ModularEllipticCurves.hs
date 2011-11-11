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
       
isValidEllipticCurve (MEC x y)
  = discri == (mod_zero embedding)
  where embedding = ringEmbedding x
        num_4     = embed 4 embedding
        num_27    = embed 27 embedding
        discri    = (mod_mult num_4 (mod_pow x 3)) `mod_add` (mod_mult num_27 (mod_pow y 2))

{-
The cubic law is based on the following:
slope = (yp - yq) / (xp - xq) if p != q
        (3 xp ^ 2 + a) / 2 yp if p == q

xr = -xp - xq + s ^ 2
yr = yp + s ( xr - xp)

where p + q = r. This is over normal space
Assume p = (xp,yp,1)
       q = (xq,yq,1)
       r = (xr,yr,1)
So we can remove the division by (xp - xq)

But over a normal ring there is no way
to consider the inverse of xp - xq
-}

-- cubicLaw ::
--   (Ring a) => ModularEllipticCurve a -> ModularProjectiveSpace a -> ModularProjectiveSpace a -> IO (ModularProjectiveSpace a)
-- cubicLaw (MEC a b) point1 point2
--   = do areTheSame <- comparePoints point1 point2
--        if areTheSame
--          then tangentApproach (MEC a b) point1 point2
--          else lineApproach (MEC a b) point1 point
              
-- tangentApproach (MEC a b) point1 point2
--   = do 
