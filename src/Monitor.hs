{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StrictData #-}

module Monitor where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef

import Pool
import Queue

------------------------------------------------------------------------

data Event = Event
  { eTimestamp   :: Int
  , eQueueLength :: Int
  , eWorkerCount :: Int
  }

monitor :: Pool -> Int -> TQueue Event -> IO ()
monitor p@(Pool _ inQueue _ _ _) dt eventQueue = go 0
  where
    go t = do
      l <- lengthQueue inQueue
      w <- readIORef (pWorkerCount p)
      print (l, w)
      atomically (writeTQueue eventQueue (Event t l w))
      threadDelay dt
      go (t + dt)
