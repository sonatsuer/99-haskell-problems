module Problem14 where

-- Problem 14
-- Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs
