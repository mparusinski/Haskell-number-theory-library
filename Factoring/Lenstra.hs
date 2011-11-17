{- |
Module      :  $Header$
Description :  Module for factorising numbers using Lenstra's ECM algorithm
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable
-}

module Factoring.Lenstra where

import EllipticCurves.ModularEllipticCurves
import Primes.Sieve

import Data.Either
import Data.Maybe
import Control.Monad
import Control.Concurrent

repeatedCubicLaw :: 
    (Integral a, Integral b) => ModularEllipticCurve a -> Point a -> b -> a -> Either (Point a) a
-- repeatedCubicLaw _ _ 0 _     = Left $ SimplePoint 0 0 -- point at infinity
repeatedCubicLaw _ point 1 _ = Left point
repeatedCubicLaw ellipticCurve point times modulus
    | result == Left Inf = Left Inf -- skip recursion
    | rem == 0           = square
    | otherwise          = squarePlus1
    where (half, rem)  = divMod times 2 
          result       = repeatedCubicLaw ellipticCurve point half modulus
          square       = either (\x -> cubicLaw ellipticCurve x x modulus) Right result
          squarePlus1  = either (\x -> cubicLaw ellipticCurve x point modulus) Right square

lenstraECMSmartBound number
    = lenstraECM number (smartBound number)

lenstraECM :: Integer -> Integer -> Maybe Integer
lenstraECM number bound 
    | number `mod` 2 == 0 = Just 2
    | number `mod` 3 == 0 = Just 3
    | number `mod` 5 == 0 = Just 5
    | otherwise           = 
        let primes      = eratosthenesSieve bound
            primePowers = map (findHighestPower bound) primes
            upperBound  = (number - 1)
            curves      = map (\x -> MEC x 1) [1..upperBound]
            initPoint   = Point 0 1
        in lenstraECMLoop number primePowers curves
    where lenstraECMLoop _ _ [] = Nothing
          lenstraECMLoop number primePowerList (ec:ecs)
              = if isNothing result then recurse else result
              where result  = lenstraECMTryEC number primePowerList (Point 0 1) ec
                    recurse = lenstraECMLoop number primePowerList ecs
          lenstraECMTryEC _ [] _ _ = Nothing
          lenstraECMTryEC number (p:ps) accumPoint ec
              = either recurse Just result
              where recurse point = lenstraECMTryEC number ps point ec
                    result        = repeatedCubicLaw ec accumPoint p number

findHighestPower n p
    = findHighestPowerAccum bound p 1
    where bound = floor $ (fromIntegral n :: Double) / (fromIntegral p :: Double)
          findHighestPowerAccum bound p accum 
              | accum > bound = accum
              | otherwise     = findHighestPowerAccum bound p (accum * p)
 
smartBound number
    = ceiling $ (l number) ** (1 / sqrt 2)
    where l x = exp (sqrt $ log x_ * log (log x_))
              where x_ = fromIntegral x :: Double
