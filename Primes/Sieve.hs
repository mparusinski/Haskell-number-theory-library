{- |
Module      :  $Header$
Description :  Module computing primes up to a certain number using 
               Erastosthen Sieve
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Primes.Sieve where

eratosthenesSieve :: (Integral a) => a -> [a]
eratosthenesSieve bound
    = eratosthenesSieveLoop bound [2..bound]

eratosthenesSieveLoop bound (n : numbers)
    | sq n > bound   = n : numbers
    | otherwise      = n : rest 
    where newNumbers = filter (\x -> mod x n /= 0) numbers
          rest       = eratosthenesSieveLoop bound newNumbers

sq x = x * x