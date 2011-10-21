{- |
Module      :  $Header$
Description :  Module to obtain continued fractions from a given number
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module ContinuedFractions where

import Data.Ratio
import ModularArithmetic.Standard

data ContinuedFraction a = ContinuedFraction [a]
                           deriving (Eq, Show)

approxContinuedFraction :: (Integral a) => Ratio a -> Ratio a -> ContinuedFraction a
approxContinuedFraction ratio1 ratio2
    = ContinuedFraction $ reverse $ approxContinuedFractionMain ratio1 ratio2

approxContinuedFractionMain :: (Integral a) => Ratio a -> Ratio a -> [a]
approxContinuedFractionMain ratio1 ratio2
    = loopApproxContinuedFraction ratio1 ratio2 []

loopApproxContinuedFraction ratio1 ratio2 accum
    | r1 < 0 || r1 >= b1 = q0 : accum
    | otherwise          = loopApproxContinuedFraction newRatio1 newRatio2 (q0:accum)
    where (q0, r0)  = euclideanDivision a0 b0
          a0        = numerator ratio1
          b0        = denominator ratio1
          a1        = numerator ratio2
          b1        = denominator ratio2
          r1        = a1 - b1 * q0
          q1        = floor (fromIntegral a1 / fromIntegral b1)
          newRatio1 = b0 % r0
          newRatio2 = b1 % r1
