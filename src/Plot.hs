module Plot where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import Monitor

------------------------------------------------------------------------

plot :: (Fractional a, Show a) => a -> a -> a -> [Event] -> IO FilePath
plot kP kI kD events = do
  let file = "/tmp/elastically-scalable-thread-pools-" ++ show kP ++ "-" ++ show kI ++ "-" ++ show kD ++ ".svg"
  toFile def file $ do
    layoutlr_title .= "Kp = " ++ show kP ++ ", Ki = " ++ show kI ++ ", Kd = " ++ show kD
    layoutlr_left_axis  . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide

    plotLeft  (line "Queue length" [ [ (t, v) | Event t v _ <- events ] ])
    plotRight (line "Worker count" [ [ (t, v) | Event t _ v <- events ] ])
  putStrLn file
  return file
