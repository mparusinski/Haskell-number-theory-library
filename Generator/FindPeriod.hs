{- |
Module      :  $Header$
Description :  Module to find periods in generators
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Generator.FindPeriod where

import Generator.Generator
import Polynomial.PolynomialGenerator
import Polynomial.Standard

findStepsUntilEquality :: (Eq a) => Generator g (a,a) -> g -> Integer
findStepsUntilEquality generator state
    = findStepsUntilEqualityCounter generator state 1

findStepsUntilEqualityCounter generator state n
    | left == right = n
    | otherwise     = findStepsUntilEqualityCounter generator nextState (n+1)
    where ((left, right), nextState) = generate generator state

findMultipleOfPeriod :: (Eq a) => Generator g a -> g -> Integer
findMultipleOfPeriod turtleGenerator initialState
    = findStepsUntilEquality doubleGenerator (initialState, initialState)
    where rabbitGenerator = makeGeneratorSkipNSteps 2 turtleGenerator
          doubleGenerator = combineGenerators turtleGenerator rabbitGenerator