module Problem55 where

import Trees

-- Problem 55
-- Construct a completely balanced binary tree

cbalTrees :: Int -> [Tree Char]
cbalTrees 0 = [Empty]
cbalTrees n | odd n     = [Branch 'x' t1 t2 | t1 <- hts, t2 <-hts] 
            | otherwise = [Branch 'x' t1 t2 | t1 <- hts, t2 <- hts']
                          ++
                          [Branch 'x' t1 t2 | t1 <- hts', t2 <- hts]
              where h   = n `div` 2
                    hts = cbalTrees h
                    hts'= cbalTrees (h - 1)
