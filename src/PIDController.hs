{-# LANGUAGE StrictData #-}
{-# LANGUAGE NumericUnderscores #-}

module PIDController where

-- https://en.wikipedia.org/wiki/PID_controller#Pseudocode

import Control.Concurrent

import Pool
import Queue

------------------------------------------------------------------------

data PIDState = PIDState
  { setpoint     :: Double
  , kP           :: Double
  , kI           :: Double
  , kD           :: Double
  , lastError    :: Double
  , lastIntegral :: Double
  }
  deriving Show

newPIDState :: Double -> Double -> Double -> Double -> PIDState
newPIDState sp kp ki kd = PIDState sp kp ki kd 0.0 0.0

step :: Double -> Double -> PIDState -> (PIDState, Double)
step input dt s = (s', output)
  where
    err          = setpoint s - input
    proportional = err
    integral     = lastIntegral s + err * dt
    derivative   = (err - lastError s) / dt
    output       = kP s * proportional + kI s * integral + kD s * derivative
    s' = s { lastError    = err
           , lastIntegral = integral
           }

controller :: Double -> Double -> Double -> Double -> Pool -> IO ()
controller kp ki kd dt p0@(Pool _ inQueue _ _ _) = go p0 (newPIDState sp kp ki kd)
  where
    sp = 0.0

    go p s = do
      n <- lengthQueue inQueue
      let (s', o) = step (realToFrac n) dt s
      -- putStrLn ("controller, output: " ++ show o)
      p' <- if o <= -100 then scaleUp p else if o >= -20 then scaleDown p else return p
      threadDelay (round (dt * 1_000_000))
      go p' s'
