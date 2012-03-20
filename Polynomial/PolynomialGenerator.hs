{- |
Module      :  $Header$
Description :  Module for creating generators from polynomials
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Polynomial.PolynomialGenerator where

import Polynomial.Standard
import Generator.Generator

import Control.Monad.State

createPolynomialGenerator :: (Integral a) => 
                              Polynomial a -> a -> Generator a a
createPolynomialGenerator poly modulus
    = Generator $ 
      do currentNumber <- get
         let nextNumber = evaluateAtModular poly currentNumber modulus
         put nextNumber
         return nextNumber