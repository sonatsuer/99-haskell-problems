module Problem23 where

import RandomPick (pickFrom)
import Control.Monad (liftM2)

-- Problem 23
-- Extract a given number of randomly selected elements from a list.

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
    | n < 0     = error "Negative value!!"
    | n == 0    = return []
    | otherwise = liftM2 (:) (pickFrom xs) (rndSelect xs (n - 1))
