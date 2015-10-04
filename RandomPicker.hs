module RandomPicker where

import System.Random
import Control.Monad

pickFrom :: [a] -> IO a
pickFrom xs
  = liftM (xs !!) (randomRIO (0, length xs - 1))
