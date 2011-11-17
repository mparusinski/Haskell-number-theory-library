{- |
Module      :  $Header$
Description :  Module for handling parallel computation
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable
-}

module HaskellToolkit.Parallel where

chunkCompeteMaybes :: (a -> Maybe b) -> [a] -> Int -> IO (Maybe b)
chunkCompeteMaybes _ [] _ = return Nothing
chunkCompeteMaybes tryFunction list num
    = do let optimalNumOfThreads = num
         let chunk = take optimalNumOfThreads list
         let rest  = drop optimalNumOfThreads list
         result <- competeMaybes tryFunction chunk
         if isNothing result
         then chunkCompeteMaybes tryFunction rest num
         else return result     

-- return firt result to succeed
competeMaybes :: (a -> Maybe b) -> [a] -> IO (Maybe b)
competeMaybes tryFunction list
    = do resultVar <- newEmptyMVar
         counterVar <- newEmptyMVar
         putMVar counterVar (length list)
         let action elem
                 = do result <- return $! tryFunction elem
                      currentCounter <- takeMVar counterVar
                      if not (isNothing result)
                      then putMVar resultVar result
                      else if currentCounter == 1 -- last to do
                           then putMVar resultVar Nothing
                           else putMVar counterVar (currentCounter - 1)
         tids <- mapM (\elem -> forkIO $ action elem) list                
         result <- takeMVar resultVar
         mapM_  killThread tids
         return result
         
