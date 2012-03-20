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

type Condition a = (a -> a -> Bool)

findStepsUntilCondition :: (Eq a) => Condition a -> Generator g (a,a) -> g -> Integer
findStepsUntilCondition condition generator state
    = findStepsUntilConditionCounter condition generator state 1

findStepsUntilConditionCounter cond gen state n
    | cond left right = n
    | otherwise       = findStepsUntilConditionCounter cond gen nextState (n+1)
    where ((left, right), nextState) = generate gen state

findMultipleOfPeriod :: (Eq a) => Condition a -> Generator g a -> g -> Integer
findMultipleOfPeriod condition turtleGenerator initialState
    = findStepsUntilCondition condition doubleGenerator (initialState, initialState)
    where rabbitGenerator = makeGeneratorSkipNSteps 2 turtleGenerator
          doubleGenerator = combineGenerators turtleGenerator rabbitGenerator