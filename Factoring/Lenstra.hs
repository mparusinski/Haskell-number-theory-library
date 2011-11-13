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
repeatedCubicLaw _ _ 0 _ = Left $ SimplePoint 0 0
repeatedCubicLaw _ point 1 _ = Left point
repeatedCubicLaw ellipticCurve point times modulus
    | rem == 0  = either evenSquare Right result
    | otherwise = either oddSquare Right result
    where (half, rem)  = divMod times 2 
          result       = repeatedCubicLaw ellipticCurve point half modulus
          evenSquare p = cubicLaw ellipticCurve p p modulus
          oddSquare r  = either (\x -> cubicLaw ellipticCurve x point modulus) Right (evenSquare r)

lenstraECMSmartBound number
    = lenstraECM number (smartBound number)

lenstraECM :: (Integral a) => a -> a -> Maybe a
lenstraECM number bound 
    | number `mod` 2 == 0 = Just 2
    | number `mod` 3 == 0 = Just 3
    | number `mod` 5 == 0 = Just 5
    | otherwise           = lenstraECMLoop number bound primes primes ellipticCurves
    where primes          = eratosthenesSieve bound
          ellipticCurves  = map (\x -> MEC x 1) list
          list            = [1..upperBound]
          upperBound      = 100

lenstraECMLoop _ _ _ _ [] = Nothing
lenstraECMLoop number bound [] primeList (ec:ecs) 
    = lenstraECMLoop number bound primeList primeList ecs
lenstraECMLoop number bound (p:ps) primeList (ec:ecs)
    = either recurse Just $ repeatedCubicLaw ec initialPoint highestPowerP number
    where highestPowerP = findHighestPowerDividing p bound
          initialPoint  = SimplePoint 0 1
          recurse arg   = lenstraECMLoop number bound ps primeList (ec:ecs)

-- this is a helper function no error checking!!!!
findHighestPowerDividing p n
    | n == 1    = 1
    | rem == 0  = p * findHighestPowerDividing p quot
    | otherwise = 1
    where (quot, rem) = divMod n p

smartBound number
    = 1 + (ceiling $ (l number) ** (1 / sqrt 2))
    where l x = exp (sqrt $ log x_ * log (log x_))
              where x_ = fromIntegral x :: Double