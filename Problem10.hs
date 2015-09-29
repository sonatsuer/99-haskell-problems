module Problem10 where

import Data.List (group)

-- Problem 10
-- Run-length encoding of a list.

encode :: Eq a => [a] -> [(Int, a)]
encode xss  = [(length xs, head xs) | xs <- group xss]
