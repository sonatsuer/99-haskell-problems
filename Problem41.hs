module Problem41 where

import Problem40 (goldbach)

-- Problem 41
-- Goldbach's conjecture, list version.

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList x y = map goldbach [lb, lb + 2..ub]
  where lb = max 4 (if even x then x else x + 1)
        ub = if even y then y else y - 1
