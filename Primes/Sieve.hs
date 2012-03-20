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

import Data.Array.IO
import Control.Monad

eratosthenesSieve :: (Integral a) => a -> [a]
eratosthenesSieve bound
    = eratosthenesSieveLoop realBound [2..bound]
    where realBound = floor . sqrt . fromIntegral $ bound

eratosthenesSieveLoop bound (n : numbers)
    | n > bound = n : numbers
    | otherwise = n : rest 
    where newNumbers = filter (\x -> mod x n /= 0) numbers
          rest       = eratosthenesSieveLoop bound newNumbers

sq x = x * x

--eratosthenesSieve_io :: (Integral a) => a -> IO [a]
eratosthenesSieve_io bound
    = do arr <- newArray_ (2, bound) :: IO (IOArray Integer a)
         realBound <- return $! floor .sqrt . fromIntegral $ bound
         mapM_ (\x -> writeArray arr x x) [2..bound]
         let filterMultiplesOfLoop n currentIndex 
                 | currentIndex > bound = return ()
                 | otherwise            = action1 >> action2
                 where action1 = writeArray arr currentIndex 0
                       action2 = filterMultiplesOfLoop n (currentIndex + n)
         let filterMultiplesOf n = filterMultiplesOfLoop n (2*n)
         let inductiveLoop currentIndex 
                 | currentIndex > realBound = return ()
                 | otherwise                =
                     do element <- readArray arr currentIndex
                        if element == 0
                           then inductiveLoop (currentIndex + 1)
                           else do filterMultiplesOf element
                                   inductiveLoop (currentIndex + 1)
         inductiveLoop 2
         liftM (filter (/= 0)) $ getElems arr
