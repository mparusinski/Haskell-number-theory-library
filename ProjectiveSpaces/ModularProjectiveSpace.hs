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
data (Ring a) => ReductionType a = RT (ElementType a -> IO (ElementType a))
data (Ring a) => ModularProjectiveSpace a =
     MPS { internal  :: ElementType a,
           reduction :: ReductionType a}
     
createProjectivePoint :: 
  (Ring a) => [ModularRing a] -> ReductionType a -> IO (ModularProjectiveSpace a)
createProjectivePoint modularElements reductionFunction
  = do coordinateArray <- createCoordinates modularElements
       return $ MPS (ET coordinateArray) reductionFunction

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

reducePoint :: 
  (Ring a) => ModularProjectiveSpace a -> IO (ModularProjectiveSpace a)
reducePoint point
  = do let (RT reductionFunction) = reduction point
       let element = internal point
       newElement <- reductionFunction element
       return $ MPS newElement (RT reductionFunction)

comparePoints ::
  (Ring a) => ModularProjectiveSpace a -> ModularProjectiveSpace a -> IO Bool
comparePoints pointA pointB 
  = do redPointA <- reducePoint pointA
       redPointB <- reducePoint pointB
       let (ET coordinatesA) = internal redPointA
       let (ET coordinatesB) = internal redPointB
       (lowA, upA) <- getBounds coordinatesA
       (lowB, upB) <- getBounds coordinatesB
       if lowA /= lowB || upA /= upB 
         then return False 
         else do let f ix = do
                       coordinateA <- readArray coordinatesA ix
                       coordinateB <- readArray coordinatesB ix
                       return $ coordinateA == coordinateB
                 truthValues <- mapM f [lowA..upA]
                 return $ and truthValues
                 
getCoordinates :: 
  (Ring a) => ModularProjectiveSpace a -> IO [ModularRing a]
getCoordinates point
  = do let (ET coordinates) = internal point
       (low, up) <- getBounds coordinates
       mapM (readArray coordinates) [low..up]
       
getCoordinate ::
  (Ring a) => Int -> ModularProjectiveSpace a -> IO (ModularRing a)
getCoordinate ix point
  = do let (ET coordinates) = internal point
       readArray coordinates ix
       