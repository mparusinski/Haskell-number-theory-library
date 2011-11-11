{- |
Module      :  $Header$
Description :  Simple Main file to test stuff
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Main where

import Data.Array.IO
import Control.Monad

import AbstractAlgebra.ModularRings
import ProjectiveSpaces.ModularProjectiveSpace
import ModularArithmetic.GCD
import EllipticCurves.ModularEllipticCurves

modulo = 31

number1, number2, number3 :: ModularRing Integer
number1 = embed 2 (flip mod modulo)
number2 = embed 2 (flip mod modulo)
number3 = embed 1 (flip mod modulo)

-- this should keep an element in its simplest forme i.e. 1 ...
reductionAlgorithm (ET coordinatesArray)
  = do (low, up) <- getBounds coordinatesArray
       coordinates <- mapM (readArray coordinatesArray) [low..up]
       let values = map representant coordinates 
       let gcd = gcdOfList values
       let newValues = map (flip div gcd) values
       let newCoordinates = map (flip embed (flip mod modulo)) newValues 
       liftM ET $ createCoordinates newCoordinates

reductionFunction = RT reductionAlgorithm

point = createProjectivePoint [number1, number2, number3] reductionFunction

ellipticCurve = MEC a b
  where a = embed 2 (flip mod modulo)
        b = mod_zero (flip mod modulo)

main = do 
  realPoint <- point
  result <- evaluateAt ellipticCurve realPoint
  print result
