module Problem25 where

import Problem24 (auxDiffSelect)

-- Problem 25
-- Generate a random permutation of the elements of a list.

rndPermu :: Eq a => [a] -> IO [a]
rndPermu xs = auxDiffSelect (length xs) xs
