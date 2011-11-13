{- |
Module      :  $Header$
Description :  Module handling polynomials modulo n
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Polynomials.ModularPolynomial where

data ModularPolynomial n a = ModularPolynomial [a]
                             deriving (Eq, Show)