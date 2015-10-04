module Problem27 where

import Data.List hiding (group)
import Problem26 (combinations)

-- Problem 27
-- Group the elements of a set into disjoint subsets.
-- (There is clash with Data.List.group.)

divisions :: Int -> [a] -> [([a], [a])]
divisions n xs 
  = [(getIndices xs il, getIndices xs ir) | (il, ir) <- indexPairs]
    where  l = length xs
           indexPairs = [(cm, [0.. l - 1] \\ cm) | cm <- combinations n [0.. l - 1]]
           getIndices list ks = [list !! k | k <- ks]

group' :: [Int] -> [a] -> [[[a]]]
group' [] _ = [[]]
group' (n : ns) xs
  = [ l : simplerSolutions | (l, r) <- divisions n xs,
                             simplerSolutions <- group' ns r ]
-- Safer version.
group :: [Int] -> [a] -> [[[a]]]
group ns xs
  | sum ns /= length xs = error "Not possible!"
  | otherwise           = group' ns xs
