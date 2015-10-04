module Problem28 where

import Data.List
import Data.Ord

-- Problem 28
-- Sorting a list of lists according to length of sublists.

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)

lfsort :: [[a]] -> [[a]]
lfsort 
  = map fst . sortBy (comparing snd) . indexByFrequency
    where freq xss xs
            = length [ys | ys <- xss, length xs == length ys]
          indexByFrequency xss
            = [(xs, freq xss xs) | xs <- xss]
