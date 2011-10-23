{- |
Module      :  $Header$
Description :  Module handling polynomials
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Polynomials where

data Polynomial a = Polynomial [a]
-- for instance a x^2 + b x + c = x (a x + b) + c
-- where [c, b, a] 

stringifyPolynomial (Polynomial cs)
    = stringifyPolyLoop cs 0
    where stringifyPolyLoop [] _    = ""
          stringifyPolyLoop [c] deg 
              | c == 1    = "X^" ++ show deg
              | otherwise = show c ++ " X^" ++ show deg
          stringifyPolyLoop (c:cs) 0
              | c == 0    = rest
              | otherwise = this ++ rest
              where this  = show c ++ " + "
                    rest  = stringifyPolyLoop cs 1
          stringifyPolyLoop (c:cs) deg
              | c == 0    = rest
              | c == 1    = "X^" ++ show deg ++ " + " ++ rest
              | otherwise = this ++ rest
              where this  = show c ++ " X^" ++ show deg ++ " + "
                    rest  = stringifyPolyLoop cs (deg+1)

degree (Polynomial cs)
    = length cs - 1

evaluateAt :: (Num a) => Polynomial a -> a -> a
evaluateAt (Polynomial coeffs) number
    = evaluateAtHelper id coeffs number

evaluateAtHelper f [] _  = 0 -- default behavior
evaluateAtHelper f [c] _ = f c -- constant polynomial
evaluateAtHelper f (c:cs) x
    = f $ c + x * (evaluateAtHelper f cs x)

addPolynomials :: (Num a) => Polynomial a -> Polynomial a -> Polynomial a
addPolynomials (Polynomial coeffs1) (Polynomial coeffs2)
    = Polynomial $ pointWisePolynomialsHelper (+) coeffs1 coeffs2

substractPolynomials :: (Num a) => Polynomial a -> Polynomial a -> Polynomial a
substractPolynomials (Polynomial coeffs1) (Polynomial coeffs2)
    = Polynomial $ pointWisePolynomialsHelper (-) coeffs1 coeffs2

pointWisePolynomialsHelper _ [] [] = []
pointWisePolynomialsHelper f (c:cs) []
    = (f c 0) : pointWisePolynomialsHelper f cs []
pointWisePolynomialsHelper f [] (c:cs)
    = (f 0 c) : pointWisePolynomialsHelper f [] cs
pointWisePolynomialsHelper f (c:cs) (d:ds)
    = (f c d) : pointWisePolynomialsHelper f cs ds