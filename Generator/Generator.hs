{- |
Module      :  $Header$
Description :  Module to control number generation
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Generator.Generator where

import Control.Monad.State

newtype Generator g a = Generator { runGenerator :: State g a }
newtype Rule a        = Rule (a -> Maybe a)

identityRule = Rule f
    where f x = return x

generateBoundedRule lowBound upBound
    = Rule functionRule
    where functionRule x
              = Just $ (+) lowBound $ x `mod` (upBound - lowBound) 

evalRule (Rule f) arg = f arg

generate :: Generator g a -> g -> (a, g)
generate (Generator generator) state 
  = (runState $ generator) state

runGeneratorNTimes :: (Integral n) => n -> Generator g a -> g -> ([a], g)
runGeneratorNTimes 0 _  _= error "Can't generate no times"
runGeneratorNTimes 1 generator state
    = let (output, newState) = generate generator state
      in ([output], newState)
runGeneratorNTimes n generator state
    = (output : prevOutputs, newState)
    where (prevOutputs, prevState) = runGeneratorNTimes (n-1) generator state
          (output, newState)       = generate generator prevState

getElemFromGenerator gen g 
    = fst $ generate gen g
getListFromGenerator n gen g 
    = fst $ (runGeneratorNTimes n gen g)

generateSetFromGenerator n gen state
    = fst $ generateSetFromGeneratorAccum n gen state

generateSetFromGeneratorAccum 0 _ _  = error "Can't generate no times"
generateSetFromGeneratorAccum 1 gen state 
    = let (output, newState) = generate gen state
      in ([output], newState)
generateSetFromGeneratorAccum n gen state
    = (elem : sofar, nextState)
    where (elem, nextState)  = generate gen prevState
          (sofar, prevState) = generateSetFromGeneratorAccum (n-1) gen state

(|>) :: Rule a -> Generator g a -> Generator g a
(|>) rule generator
    = Generator $ do -- defining runGenerator  
        currentState <- get
        let (output, newState) = generate generator currentState
        put newState
        let reshapedOuput = evalRule rule output
        case reshapedOuput of
          Just someOutput -> return someOutput
          Nothing         -> runGenerator (rule |> generator)

