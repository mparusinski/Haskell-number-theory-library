{- |
Module      :  $Header$
Description :  Module to control random number generation
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module RandomGenerator where

import System.Random
import Control.Monad.State

newtype Generator g a = Generator { runGenerator :: State g a }
newtype Rule a        = Rule (a -> Maybe a)

identityRule 
    = Rule return
        
onlyOddRule 
    = Rule (\x -> if even x then Nothing else Just x)

turnOdd 
    = Rule (\x -> Just $ 2 * x + 1)

giveGoodNumber
    = Rule function
    where function x = if allConditions then Just newX else Nothing
              where newX          = 2 * x + 1
                    allConditions = cond1 && cond2
                    cond1         = newX == 5 || newX `mod` 5 /= 0
                    cond2         = newX == 3 || newX `mod` 3 /= 0

generate :: Generator g a -> g -> (a, g)
generate (Generator generator) state 
  = (runState $ generator) state

simpleGenerator :: (Integral a, Random a) => Generator StdGen a
simpleGenerator 
    = Generator $ 
      do randomGenerator <- get 
         let (output, nextGenerator) = random randomGenerator 
         put nextGenerator 
         return output

simpleBoundedGenerator :: (Integral a, Random a) => a -> a -> Generator StdGen a
simpleBoundedGenerator lowBound highBound
    = Generator $ 
      do randomGenerator <- get
         let (output, nextGenerator) = randomR (lowBound, highBound) randomGenerator
         put nextGenerator
         return output

(|>) :: Rule a -> Generator g a -> Generator g a
(|>) (Rule rule) generator
    = Generator $ do -- defining runGenerator  
        currentState <- get
        let (output, newState) = generate generator currentState
        put newState
        let reshapedOuput = rule output
        case reshapedOuput of
          Just someOutput -> return someOutput
          Nothing         -> runGenerator ((Rule rule) |> generator)

runGeneratorNTimes :: (Integral n) => n -> Generator g a -> g -> ([a], g)
runGeneratorNTimes 0 _  _= error "Can't generate no times"
runGeneratorNTimes 1 generator state
    = let (output, newState) = generate generator state
      in ([output], newState)
runGeneratorNTimes n generator state
    = (outputs, newState)
    where (prevOutputs, prevState) = runGeneratorNTimes (n-1) generator state
          (output, newState)       = generate generator prevState
          outputs                  = output : prevOutputs
