module Queue where

import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM

------------------------------------------------------------------------

newtype Queue a = Queue (TBQueue a)

newQueue :: Int -> IO (Queue a)
newQueue sz = Queue <$> newTBQueueIO (fromIntegral sz)

readQueue :: Queue a -> IO a
readQueue (Queue q) = atomically (readTBQueue q)

writeQueue :: Queue a -> a -> IO ()
writeQueue (Queue q) x = atomically (writeTBQueue q x)

lengthQueue :: Queue a -> IO Int
lengthQueue (Queue q) = do
  n <- atomically (lengthTBQueue q)
  return (fromIntegral n)
