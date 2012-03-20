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

data Polynomial a = 
    Polynomial {
      getCoeffs :: [a]
    }
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

stringifyPolynomial polynomial
    = stringifyPolyLoop (getCoeffs polynomial) 0
    where stringifyPolyLoop [] _    = ""
          stringifyPolyLoop [c] deg 
              = show c ++ " X^" ++ show deg
          stringifyPolyLoop (c:cs) 0
              = show c ++ " + " ++ stringifyPolyLoop cs 1
          stringifyPolyLoop (c:cs) deg
              = this ++ rest
              where this  = show c ++ " X^" ++ show deg ++ " + "
                    rest  = stringifyPolyLoop cs (deg+1)

negatePolynomial polynomial
  = Polynomial $ map negate $ getCoeffs polynomial

-- derived from Manhattan Norm
absOfPolynomial polynomial
  = Polynomial $ return . sum $ map abs $ getCoeffs polynomial

degree polynomial
    = length (getCoeffs polynomial) - 1

getValuationMorphism :: (Num a) => Polynomial a -> (a -> a)
getValuationMorphism polynomial
    = morphism
    where morphism x = evaluateAt polynomial x

evaluateAt :: (Num a) => Polynomial a -> a -> a
evaluateAt polynomial number
    = evaluateAtHelper id (getCoeffs polynomial) number

evaluateAtModular :: (Integral a) => Polynomial a -> a -> a -> a
evaluateAtModular polynomial number modulus
    = evaluateAtHelper (flip mod modulus) (getCoeffs polynomial) number

evaluateAtHelper f [] _  = 0 -- default behavior
evaluateAtHelper f [c] _ = f c -- constant polynomial
evaluateAtHelper f (c:cs) x
    = f $ c + x * (evaluateAtHelper f cs x)

addPolynomials :: (Num a) => Polynomial a -> Polynomial a -> Polynomial a
addPolynomials polynomial1 polynomial2
    = Polynomial $ pointWisePolynomialsHelper (+) coeffs1 coeffs2
    where coeffs1 = getCoeffs polynomial1
          coeffs2 = getCoeffs polynomial2

substractPolynomials :: (Num a) => Polynomial a -> Polynomial a -> Polynomial a
substractPolynomials polynomial1 polynomial2
    = Polynomial $ pointWisePolynomialsHelper (-) coeffs1 coeffs2
    where coeffs1 = getCoeffs polynomial1
          coeffs2 = getCoeffs polynomial2

pointWisePolynomialsHelper _ [] [] = []
pointWisePolynomialsHelper f (c:cs) []
    = (f c 0) : pointWisePolynomialsHelper f cs []
pointWisePolynomialsHelper f [] (c:cs)
    = (f 0 c) : pointWisePolynomialsHelper f [] cs
pointWisePolynomialsHelper f (c:cs) (d:ds)
    = (f c d) : pointWisePolynomialsHelper f cs ds

-- TODO: Use fourier transform technique if more efficient ...
multiplyPolynomials :: (Num a) => Polynomial a -> Polynomial a -> Polynomial a
multiplyPolynomials polynomial1 polynomial2
    = Polynomial $ multiplyPolynomialsHelper coeffs1 coeffs2
    where coeffs1 = getCoeffs polynomial1
          coeffs2 = getCoeffs polynomial2
      
scalarMultiply polynomialCoefficients constant
    = map (constant *) polynomialCoefficients
      
multiplyPolynomialsHelper :: (Num a) => [a] -> [a] -> [a]
-- ERROR HANDLING IS A PERFORMANCE ISSUE
-- SHOULD INSTEAD MAKE IMPOSSIBLE TO GET TO THIS SITUATION
--multiplyPolynomialsHelper [] _ = error "Empty polynomial"
--multiplyPolynomialsHelper _ [] = error "Empty polynomial"
multiplyPolynomialsHelper [c] poly
    = scalarMultiply poly c
multiplyPolynomialsHelper poly [c]
    = scalarMultiply poly c
multiplyPolynomialsHelper (c:cs) (d:ds)
    = foldl1 (pointWisePolynomialsHelper (+)) terms 
    where constant  = [c*d]
          left      = 0 : scalarMultiply ds c
          right     = 0 : scalarMultiply cs d
          recursive = 0 : 0 : multiplyPolynomialsHelper cs ds
          terms     = [constant, left, right, recursive]
          