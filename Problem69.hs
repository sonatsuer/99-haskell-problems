module Problem69 where

import Control.Applicative
import MiniParse
import Trees

-- Problem 69
-- Dotstring representation of binary trees.

-- Convert a dot-string representation into a tree using
-- the dotSTrP parser.
dstringToTree :: String -> Maybe (Tree Char)
dstringToTree s = parse (final dotStrP) s

-- A tree is either empty or nonempty.
dotStrP :: Parser (Tree Char)
dotStrP = emptyTreeP <|> nonemptyTreeP

nonemptyTreeP :: Parser (Tree Char)
nonemptyTreeP = do x <- nonDotP
                   t1 <- dotStrP
                   t2 <- dotStrP
                   return $ Branch x t1 t2

nonDotP :: Parser Char
nonDotP = satisfy (/= '.')
  
emptyTreeP :: Parser (Tree Char)
emptyTreeP = do x <- char '.'
                return Empty
