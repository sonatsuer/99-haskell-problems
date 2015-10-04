module Problem17 where

-- Problem 17
-- Split a list into two parts; the length of the
-- first part is given.

split :: [a] -> Int -> ([a], [a])
split xs n  = 
  let ps = zip xs [1..]
      l  = [x | (x, i) <- ps, i <= n]
      r  = [x | (x, i) <- ps, i > n]
  in (l, r)
