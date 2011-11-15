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
    | rem == 0                  = square
    | otherwise                 = squarePlus1
    where (half, rem)  = divMod times 2 
          result       = repeatedCubicLaw ellipticCurve point half modulus
          square       = either (\x -> cubicLaw ellipticCurve x x modulus) Right result
          squarePlus1  = either (\x -> cubicLaw ellipticCurve x point modulus) Right square
          pointAtInf   = SimplePoint 0 0

lenstraECMSmartBound number
    = lenstraECM number (smartBound number)

lenstraECM :: Integer -> Integer -> IO (Maybe Integer)
lenstraECM number bound 
    | number `mod` 2 == 0 = return $ Just 2
    | number `mod` 3 == 0 = return $ Just 3
    | number `mod` 5 == 0 = return $ Just 5
    | otherwise           = 
        do primes <- eratosthenesSieve_io bound
           primePowers <- return $! map (findHighestPower bound) primes
           return $ lenstraECMLoop number primePowers primePowers ellipticCurves initialPoint
    where ellipticCurves  = map (\x -> MEC x 1) list
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