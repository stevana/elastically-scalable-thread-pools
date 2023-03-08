module LoadGenerator where

import Control.Concurrent
import Control.Monad

import Queue

------------------------------------------------------------------------

generator :: Queue a -> a -> Double -> Double -> Double -> IO ()
generator q x i dt t = go 0
  where
    go s | s >= t    = return ()
         | otherwise = do
             let n = sin s * i + i
             replicateM_ (round n) (writeQueue q x >> threadDelay (round (dt * 1e6 / n)))
             -- l <- lengthQueue q
             -- putStrLn $ "generator, dt: " ++ show dt ++ ", n: " ++ show n ++ ", delay: " ++
             --   show (round (dt * 1e6 / n)) ++ ", queue length: " ++ show l
             go (s + dt)
