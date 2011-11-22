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

repeatedParallelCubic ::
    (Integral a, Integral b) => [ModularEllipticCurve a] -> [Point a] -> b -> a -> Either [Point a] a
repeatedParallelCubic _ points 1 _ = Left points
repeatedParallelCubic ecs points times modulus
    | rem == 0  = square
    | otherwise = squarePlus1
    where (half, rem) = divMod times 2
          result      = repeatedParallelCubic ecs points half modulus
          square      = case result of
                          Left ps -> parallelCubicLaw ecs ps ps modulus
                          Right div -> Right div
          squarePlus1 = case square of
                          Left ps -> parallelCubicLaw ecs points ps modulus
                          Right div -> Right div

lenstraECMSmartBound number
    = lenstraECM number (smartBound number)

lenstraECMParallelSmartBound number
    = lenstraECMParallel number (smartBound number)

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
        in lenstraECMLoop number primePowers curves
    where lenstraECMLoop _ _ [] = Nothing
          lenstraECMLoop number primePowers (ec:ecs)
              = if isNothing result then recurse else result
              where result  = lenstraECMTryEC number primePowers initP ec
                    recurse = lenstraECMLoop number primePowers ecs
                    initP   = Point 0 1
          lenstraECMTryEC _ [] _ _ = Nothing
          lenstraECMTryEC number (p:ps) accumPoint ec
              = either recurse Just result
              where recurse point = lenstraECMTryEC number ps point ec
                    result        = repeatedCubicLaw ec accumPoint p number


-- Good chunksize was obtained from experimentation
lenstraECMParallel :: Integer -> Integer -> Maybe Integer
lenstraECMParallel number bound
    | number `mod` 2 == 0 = Just 2
    | number `mod` 3 == 0 = Just 3
    | number `mod` 5 == 0 = Just 5
    | otherwise           =
        let primes      = eratosthenesSieve bound
            primePowers = map (findHighestPower bound) primes
            upperBound  = (number - 1)
            curves      = map (\x -> MEC x 1) [1..upperBound]
        in lenstraECMLoop number primePowers curves
    where lenstraECMLoop _ _ [] = Nothing
          lenstraECMLoop number primePowers ecs
              = if isNothing result then recurse else result
              where result    = lenstraECMTryChunk number primePowers points ecChunk
                    recurse   = lenstraECMLoop number primePowers ecRest
                    ecChunk   = take chunkSize ecs
                    ecRest    = drop chunkSize ecs
                    points    = take chunkSize $ repeat (Point 0 1)
                    chunkSize = if number < 100 then fromIntegral number else 100
          lenstraECMTryChunk _ [] _ _ = Nothing
          lenstraECMTryChunk number (p:ps) accumPoints ecChunk
              = either recurse Just result
              where recurse points = lenstraECMTryChunk number ps points ecChunk
                    result         = repeatedParallelCubic ecChunk accumPoints p number

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
