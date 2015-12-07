module Problem70 where

import Control.Applicative
import MiniParse
import Trees

-- Probelm 70.
-- Tree construction from a node string.


treeToString :: MTree Char -> String
treeToString = (++"^") . mTreeFold f
  where f x strs = x : concatMap (++"^") strs


stringToTree :: String -> Maybe (MTree Char)
stringToTree s = parse (final treeP) s


treeP :: Parser (MTree Char)
treeP = do x <- nonUpP
           ts <- many treeP
           _ <- char '^'
           return $ Node x ts

nonUpP :: Parser Char
nonUpP = satisfy (/='^')

