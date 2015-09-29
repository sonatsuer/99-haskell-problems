module Problem8 where

-- Problem 8
-- Eliminate consecutive duplicates of list elements.

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x1 : x2 : xs) =
  if x1 == x2 then rest else x1 : rest where
    rest = compress (x2 : xs)
