{-# LANGUAGE ExistentialQuantification #-}

module Pool where

import Control.Concurrent.Async
import Data.IORef

import Queue

------------------------------------------------------------------------

data Pool = forall a b. Pool
  { pWorker      :: a -> IO b
  , pInQueue     :: Queue a
  , pOutQueue    :: Queue b
  , pWorkers     :: [Async ()]
  , pWorkerCount :: IORef Int
  }

newPool :: (a -> IO b) -> Queue a -> Queue b -> IO Pool
newPool worker inQueue outQueue = Pool worker inQueue outQueue [] <$> newIORef 0

scaleUp :: Pool -> IO Pool
scaleUp (Pool worker inQueue outQueue pids count) = do
  putStrLn "scaling up"
  pid <- async go
  modifyIORef' count (+ 1)
  return (Pool worker inQueue outQueue (pid : pids) count)
  where
    go = do
      x <- readQueue inQueue
      y <- worker x
      writeQueue outQueue y
      go

scaleDown :: Pool -> IO Pool
scaleDown p = case pWorkers p of
  []         -> return p
  pid : pids -> do
    putStrLn "scaling down"
    cancel pid
    modifyIORef' (pWorkerCount p) (\i -> i - 1)
    return p { pWorkers = pids }
