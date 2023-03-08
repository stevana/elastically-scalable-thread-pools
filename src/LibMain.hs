{-# LANGUAGE NumericUnderscores #-}

module LibMain where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Concurrent.STM
import System.Process

import LoadGenerator
import Pool
import Queue
import Monitor
import Plot
import PIDController

------------------------------------------------------------------------

libMain :: IO ()
libMain = do
  q  <- newQueue 65536
  q' <- newQueue 65536
  evQ <- newTQueueIO
  let worker _ = threadDelay 25000
  p  <- newPool worker q q'
  putStrLn "Start generating"
  ag <- async (generator q () 60 0.1 60)
  am <- async (monitor p 500000 evQ)
  let kp = 1
      ki = 0.05
      kd = 0.05
      dt = 0.01
  ac <- async (controller kp ki kd dt p)
  wait ag
  putStrLn "Done generating, waiting for queue to empty..."
  threadDelay 500_000
  mapM_ cancel  [am, ac]
  events <- atomically (flushTQueue evQ)
  putStr "Plotting to: "
  fp <- plot kp ki kd events
  callProcess "firefox" [fp]
