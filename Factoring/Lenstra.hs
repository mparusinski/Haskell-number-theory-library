{- |
Module      :  $Header$
Description :  Module for factorising numbers using Lenstra's algorithm
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

repeatedCubicLaw :: 
    (Integral a, Integral b) => ModularEllipticCurve a -> SimplePoint a -> b -> a -> Either (SimplePoint a) a
-- repeatedCubicLaw _ _ 0 _     = Left $ SimplePoint 0 0 -- point at infinity
repeatedCubicLaw _ point 1 _ = Left point
repeatedCubicLaw ellipticCurve point times modulus
    | result == Left pointAtInf = Left pointAtInf -- skip recursion
    | rem == 0                  = either evenSquare Right result
    | otherwise                 = either oddSquare Right result
    where (half, rem)  = divMod times 2 
          result       = repeatedCubicLaw ellipticCurve point half modulus
          evenSquare p = cubicLaw ellipticCurve p p modulus
          oddSquare r  = either (\x -> cubicLaw ellipticCurve x point modulus) Right (evenSquare r)
          pointAtInf   = SimplePoint 0 0

lenstraECMSmartBound number
    = lenstraECM number (smartBound number)

lenstraECM :: (Integral a) => a -> a -> Maybe a
lenstraECM number bound 
    | number `mod` 2 == 0 = Just 2
    | number `mod` 3 == 0 = Just 3
    | number `mod` 5 == 0 = Just 5
    | otherwise           = lenstraECMLoop number primePowers primePowers ellipticCurves initialPoint
    where primePowers     = map (flip findHighestPower bound) $ eratosthenesSieve bound
          ellipticCurves  = map (\x -> MEC x 1) list
          list            = [1..upperBound]
          upperBound      = number - 1
          initialPoint    = SimplePoint 0 1

{-
Try avoiding recomputing the highestpowers up to b
-}
lenstraECMLoop _ _ _ [] _ = Nothing
lenstraECMLoop number [] primePowerList (ec:ecs) _
    = lenstraECMLoop number primePowerList primePowerList ecs initialPoint
    where initialPoint  = SimplePoint 0 1 
lenstraECMLoop number (p:ps) primeList (ec:ecs) accumPoint
    = either recurse Just result
    where recurse point = lenstraECMLoop number ps primeList (ec:ecs) point
          result        = repeatedCubicLaw ec accumPoint p number

-- this is a helper function no error checking!!!!
findHighestPower :: (Integral a) => a -> a -> a
findHighestPower p n
    = findHighestPowerAccum p bound 1
    where bound = floor $ (fromIntegral n :: Double) / (fromIntegral p :: Double)
          findHighestPowerAccum p bound accum 
              | accum > bound = accum
              | otherwise     = findHighestPowerAccum p bound (accum * p)

smartBound number
    = ceiling $ (l number) ** (1 / sqrt 2)
    where l x = exp (sqrt $ log x_ * log (log x_))
              where x_ = fromIntegral x :: Double