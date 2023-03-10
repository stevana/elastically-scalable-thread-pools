module Main where

import System.Environment
import System.Exit

import LibMain

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (kp, ki, kd) <- case args of
                    [kp, ki, kd] -> return (read kp, read ki, read kd)
                    _otherwise   -> do
                      putStrLn "Usage: needs to be called with three arguments"
                      exitFailure
  libMain kp ki kd
