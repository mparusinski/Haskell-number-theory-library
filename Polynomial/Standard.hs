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

module Polynomial.Standard where

data Polynomial a = Polynomial [a]
                    deriving (Eq)
-- for instance a x^2 + b x + c = x (a x + b) + c
-- where [c, b, a] 

instance (Show a, Num a) => Show (Polynomial a) where
  show poly = stringifyPolynomial poly
         
instance (Num a) => Num (Polynomial a) where
  (+) = addPolynomials
  (-) = substractPolynomials
  (*) = multiplyPolynomials
  negate = negatePolynomial
  abs    = absOfPolynomial 
  signum = Polynomial . return . fromIntegral . degree
  fromInteger = Polynomial . return . fromInteger

stringifyPolynomial (Polynomial cs)
    = stringifyPolyLoop cs 0
    where stringifyPolyLoop [] _    = ""
          stringifyPolyLoop [c] deg 
              = show c ++ " X^" ++ show deg
          stringifyPolyLoop (c:cs) 0
              = show c ++ " + " ++ stringifyPolyLoop cs 1
          stringifyPolyLoop (c:cs) deg
              = this ++ rest
              where this  = show c ++ " X^" ++ show deg ++ " + "
                    rest  = stringifyPolyLoop cs (deg+1)

negatePolynomial (Polynomial cs)
  = Polynomial $ map negate cs

-- derived from Manhattan Norm
absOfPolynomial (Polynomial cs)
  = Polynomial $ return . sum $ map abs cs

degree (Polynomial cs)
    = length cs - 1

evaluateAt :: (Num a) => Polynomial a -> a -> a
evaluateAt (Polynomial coeffs) number
    = evaluateAtHelper id coeffs number

evaluateAtModular :: (Integral a) => Polynomial a -> a -> a -> a
evaluateAtModular (Polynomial coeffs) number modulus
    = evaluateAtHelper (flip mod modulus) coeffs number

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

-- TODO: Use fourier transform technique if more efficient ...
multiplyPolynomials :: (Num a) => Polynomial a -> Polynomial a -> Polynomial a
multiplyPolynomials (Polynomial poly1) (Polynomial poly2)
    = Polynomial $ multiplyPolynomialsHelper poly1 poly2
      
scalarMultiply polynomialCoefficients constant
    = map (constant *) polynomialCoefficients
      
multiplyPolynomialsHelper :: (Num a) => [a] -> [a] -> [a]
multiplyPolynomialsHelper [] [] = error "Undefined empty polynomial"
multiplyPolynomialsHelper [c] poly
    = scalarMultiply poly c
multiplyPolynomialsHelper poly [c]
    = scalarMultiply poly c
multiplyPolynomialsHelper (c:cs) (d:ds)
    = foldl1 (pointWisePolynomialsHelper (+)) [constantTerm, leftTerm, rightTerm, recursiveTerm]
    where constantTerm  = [c*d]
          leftTerm      = 0 : scalarMultiply ds c
          rightTerm     = 0 : scalarMultiply cs d
          recursiveTerm = 0 : 0 : multiplyPolynomialsHelper cs ds
          