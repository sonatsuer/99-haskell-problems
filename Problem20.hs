module Problem20 where

-- Problem 20
-- Remove the K'th element from a list.

removeAt :: [a] -> Int -> (a, [a])
removeAt xs n
  = let l = xs !! (n - 1)
        r = take (n - 1) xs ++ drop n xs
    in (l, r)
