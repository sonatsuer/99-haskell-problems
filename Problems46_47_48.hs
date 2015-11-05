module Problems46_47_48 where

-- Problem 48
-- Constructing and printing truth tables.
-- Problems 46 and 47 are trivial.

possibilities :: Int -> [[Bool]]
possibilities 0 = [[]]
possibilities n = firstTrue ++ firstFalse
  where firstTrue  = [True : ls | ls <- smaller]
        firstFalse = [False : ls | ls <- smaller]
        smaller    = possibilities (n - 1)


printLines :: [String] -> IO ()
printLines xs = sequence_  [putStr (x ++ "\n") | x <- xs]

table :: Int -> ([Bool] -> Bool) -> [String]
table n op = [ show ps ++ ":" ++ show (op ps) | ps <- possibilities n]

tableDisplay :: Int -> ([Bool] -> Bool) -> IO ()
tableDisplay n op = printLines $ table n op
