module Problem7 where

-- Problem 7
-- Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List ls) = concatMap flatten ls
