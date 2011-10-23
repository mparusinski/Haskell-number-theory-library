{- |
Module      :  $Header$
Description :  Creates a generator of increasing or decreasing number from generator
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Generator.MonotonicGenerator where

import Control.Monad.State
import Generator.Generator
import Generator.RandomGenerator

data Sign = Negative | Positive

realSign Negative = -1
realSign Positive =  1

-- PRE: assuming we are given a generator of small positive numbers
createMonotonicGenerator :: (Num a) => Sign -> Generator g a -> Generator (g,a) a
createMonotonicGenerator sign smallNumGenerator
    = Generator $ do
        let actualSign         = realSign sign
        (currentState, prevNumber) <- get
        let (output, newState) = generate smallNumGenerator currentState
        let newNumber          =  prevNumber + actualSign * output
        put (newState, newNumber)
        return newNumber