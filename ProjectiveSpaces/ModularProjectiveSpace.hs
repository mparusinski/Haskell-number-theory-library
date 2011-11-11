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

import ModularArithmetic.GCD

{- 
Idea we have ring R and ideal I with embedding f
We define the modular ring R/I obtained from f
From R/I we define a projective space by a n-tuple

WARNING: This code is not pure
-}

{- 
NOTE: I have removed the reduction bit It is better for the user of
this module to handle explicitly the elements and fqor him to choose
what representation to use

WARNING: This class doesn't take care of all operations. This should
be more seen like an interface to projective spaces 
-}

data (Ring a) => ModularProjectiveSpace a =
     MPS (IOArray Int (ModularRing a))
     
createProjectivePoint :: (Ring a) => [ModularRing a] -> IO (ModularProjectiveSpace a)
createProjectivePoint modularElements
  = do coordinateArray <- createCoordinates modularElements
       return $ MPS coordinateArray

createCoordinates coordinatesList
  = do let dimension = length coordinatesList
       arr <- newArray_ (1,dimension)
       counter <- newIORef 1
       let f elem = do
             current <- readIORef counter
             writeArray arr current elem
             writeIORef counter (current+1)
       mapM_ f coordinatesList
       return arr
                 
getCoordinates :: 
  (Ring a) => ModularProjectiveSpace a -> IO [ModularRing a]
getCoordinates (MPS point)
  = do (low, up) <- getBounds point
       mapM (readArray coordinates) [low..up]
       
getCoordinate ::
  (Ring a) => Int -> ModularProjectiveSpace a -> IO (ModularRing a)
getCoordinate ix (MPS point)
  = readArray point ix
       