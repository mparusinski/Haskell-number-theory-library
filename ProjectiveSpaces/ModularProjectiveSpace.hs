{- |
Module      :  $Header$
Description :  Module modular projective space
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

This is designed for projective space over (Z/NZ)
But maybe it can be generalised
-}

module ProjectiveSpaces.ModularProjectiveSpace where

import Data.Array.IO
import Data.IORef
import Data.Ix

import Control.Monad

import AbstractAlgebra.Rings
import AbstractAlgebra.ModularRings

{- 
Idea we have ring R and ideal I with embedding f
We define the modular ring R/I obtained from f
From R/I we define a projective space by a n-tuple

WARNING: This code is not pure
-}

data (Ring a) => ElementType a   = ET (IOArray Int (ModularRing a))
data (Ring a) => ReductionType a = RT (ElementType a -> ElementType a)
data (Ring a) => ModularProjectiveSpace a =
     MPS { element   :: ElementType a,
           reduction :: ReductionType a}
     
createProjectiveElement :: 
  (Ring a) => [ModularRing a] -> ReductionType a -> IO (ModularProjectiveSpace a)
createProjectiveElement modularElements reductionFunction
  = do let dimension = length modularElements 
       arr <- newArray_ (1,dimension)
       counter <- newIORef 1 
       let f elem = do 
             current <- readIORef counter
             writeArray arr current elem 
             writeIORef counter (current+1)
       mapM_ f modularElements
       return $ MPS (ET arr) reductionFunction

     
