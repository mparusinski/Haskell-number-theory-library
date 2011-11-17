{- |
Module      :  $Header$
Description :  Module for elliptic curves over Z/NZ (hence modulus)
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
data (Integral a) => Point a = Inf | Point a a
                                   deriving (Show, Eq)
data (Integral a) => ResultPoint a = Either (Point a) a

evaluateFunction a b x y t n
  = term1 - term2 - term3 - term4
  where term1 = (y * y * t) `mod` n
        term2 = (x * x * x) `mod` n
        term3 = (a * x * t * t) `mod` n
        term4 = (b * t * t * t) `mod` n
       
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
-}

-- PRE: Assuming zp = 1, zq =1
--      We assume xp,yp is reduced modulo n same for xq,yq
-- Norm is for Normal
-- Tang is for Tangent
cubicLaw _ Inf p _ = Left p
cubicLaw _ p Inf _ = Left p
cubicLaw (MEC a b) (Point xp yp) (Point xq yq) n
    | xp /= xq  = let (gcd, inv, rest) = extendedEuclid (xp - xq) n
                      slope            = ((yp - yq) * (inv `mod` n)) `mod` n
                      xr               = (slope * slope - xp - xq) `mod` n
                      yr               = xr `seq` (yp + slope * (xr - xp)) `mod` n
                 in if gcd > 1 then Right gcd else yr `seq` Left $ Point xr yr
    | yp == -yq = Left Inf
    | otherwise = let (gcd, inv, rest) = extendedEuclid (2 * yp) n
                      slope            = mod (((3 * xp * xp + a) `mod` n) * (inv `mod` n)) n
                      xr               = (slope * slope - 2 * xp) `mod` n
                      yr               = xr `seq` (yp + slope * (xr -xp)) `mod` n
                  in if gcd > 1 then Right gcd else yr `seq` Left $ Point xr yr
          