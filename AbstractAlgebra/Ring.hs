{- |
Module      :  $Header$
Description :  Module handling rings
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module AbstractAlgebra.Rings where

-- TODO: Create a sensible type class if possible

-- LAWS SATISFIED:
--   _ getZero + x = x + getZero = x
--   _ getOne * x = x * getOne = x
--   _ (x + y) + z = x + (y + z)
--   _ x + negate y = x - y
--   _ negate x + y = y - x
--   _ x - x = getZero
--   _ (x * y) * z = x * (y * z)
class (Eq a) => Ring a where
    (+) :: a -> a -> a
    negate :: a -> a
    (-) :: a -> a -> a 
    (*) :: a -> a -> a
    getZero :: a
    getOne  :: a
-- (-) could be express in terms of negate but efficiency we do not
--     better to require an explicit definition
    
class (Ring a) => EuclideanDomain a where
    euclideanDivision :: a -> a -> (a, a)

-- instance (EuclideanDomain a) => Integral a where
--     quot x y = fst $ euclideanDivision x y
--     rem  x y = snd $ euclideanDivision x y
--     div = quot
--     mod = rem

-- LAWs SATISFIED:
--   TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
class (Ring a) => Field a where
    (/) :: a -> a -> a -- x / 0 yields error
    getInverse :: a -> a -- getInverse 0 yields error



