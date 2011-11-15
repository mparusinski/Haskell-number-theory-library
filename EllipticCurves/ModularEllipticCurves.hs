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

import Data.Either

import AbstractAlgebra.Rings
import AbstractAlgebra.ModularRings

import ModularArithmetic.GCD

-- Recall a elliptic curves over a ring/field that 
-- do not have characteristic divisible by 2 or 3
-- Y^2 * T = X^3 + A*X * T^2 + B * T^3
-- Elements within it are [X:Y:Z] satisfying the 
-- above.
-- This is only defined over intergers

data (Integral a) => ModularEllipticCurve a = MEC a a
data (Integral a) => Point a = Point a a a
                             deriving (Show)
data (Integral a) => SimplePoint a = SimplePoint a a
                                   deriving (Show, Eq)
data (Integral a) => ResultPoint a = Either (SimplePoint a) a

evaluateFunction a b x y t n
  = term1 - term2 - term3 - term4
  where term1 = (y * y * t) `mod` n
        term2 = (x * x * x) `mod` n
        term3 = (a * x * t * t) `mod` n
        term4 = (b * t * t * t) `mod` n

evaluateAt :: 
  (Integral p) => ModularEllipticCurve p -> Point p -> p -> p
evaluateAt (MEC a b) (Point x y t) n
  = evaluateFunction a b x y t n
       
isValidEllipticCurve (MEC x y) n
  = discriminant /= 0
  where discriminant = ((4 * x * x * x) `mod` n) + ((27 * y * y) `mod` n)

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

-- PRE: Assuming zp = 1, zq =1
--cubicLaw _ (SimplePoint 0 0) a = a
--cubicLaw _ a (SimplePoint 0 0) = a
cubicLaw (MEC a b) (SimplePoint xp yp) (SimplePoint xq yq) n
    | factorNorm /= 0      = if gcdNorm > 1 
                             then Right gcdNorm 
                             else Left (resultPoint normalSlope)
    | mod (yp + yq) n == 0 = Left $ SimplePoint 0 0 -- convention for point at infinity 
    | otherwise            = if gcdTang > 1 
                             then Right gcdTang
                             else Left (resultPoint tangentSlope)
    where (gcdNorm, iNorm, kNorm) = extendedEuclid factorNorm n
          (gcdTang, iTang, kTang) = extendedEuclid factorTang n
          factorNorm              = mod (xp - xq) n
          factorTang              = mod (2 * yp) n
          normalSlope             = ((yp - yq) * iNorm) `mod` n
          tangentSlope            = ((3 * xp * xp + a) * iTang) `mod` n
          xr slope                = (slope * slope - xp - xq) `mod` n
          yr slope                = (yp + slope * ((xr slope) - xp)) `mod` n
          resultPoint slope       = SimplePoint (xr slope) (yr slope)
          
          