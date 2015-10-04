module Problem26 where

-- Problem 26
-- Generate the combinations of K distinct objects 
-- chosen from the N elements of a list.

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs
  = [x : zs | (x, ys) <- simplerCases, zs <- combinations (n - 1) ys]
    where simplerCases
            = [(xs !! (i - 1), drop i xs) | i <-[1.. length xs]]
