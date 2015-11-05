module Problem49 where

-- Problem 49
-- Consrtucting Gray Codes.

gray :: Int -> [String]
gray 0 = [[]]
gray n = first1 ++ first0
  where first1 = ['1' : xs | xs <- gray (n - 1)]
        first0 = ['0' : xs | xs <- reverse (gray (n - 1))]
