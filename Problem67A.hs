module Problem67A where

import Control.Applicative
import MiniParse
import Trees
import DList

-- Problem 67A
-- A string representation of binary trees.


-- Converts a tree into its string representation.
treeToString :: Tree Char -> String
treeToString = toList . aux where
  aux t = case t of
          Empty
            -> nilD
          Branch c Empty Empty
            -> singleton c
          Branch c t1 t2
            -> singleton c +++
               singleton '(' +++
               aux t1 +++
               singleton ',' +++
               aux t2 +++
               singleton ')'


-- Converts a string into a tree, using the treeP parser,
-- if possible.
stringToTree :: String -> Maybe (Tree Char)
stringToTree s = parse treeP s


-- There is no unnecessary characters at the end of the string.
treeP :: Parser (Tree Char)
treeP = final emptyTreeP <|> final nonemptyTreeP

emptyTreeP :: Parser (Tree Char)
emptyTreeP = return Empty


-- Here singleNode should be the last option
-- as "x" is an initial segment of "x(...".
nonemptyTreeP :: Parser (Tree Char)
nonemptyTreeP = branchP <|> singleNodeP

singleNodeP :: Parser (Tree Char)
singleNodeP = do x <- nonPunctuaution
                 return $ Branch x Empty Empty

nonPunctuaution :: Parser Char
nonPunctuaution =
  satisfy $ \ c -> not (c `elem` "(,)")

branchP :: Parser (Tree Char)
branchP = do c  <- nonPunctuaution
             _  <- char '('
             (t1, t2) <- bRight <|> bBoth <|> bLeft
             _ <- char ')'
             return $ Branch c t1 t2

-- Every tree has a unique representation as a string.
-- If you relax this an parse both "a" and "a(,)" as
-- Branch a Empty Empty then the parser can be simplified.
             
-- Branching right.
bRight :: Parser (Tree Char, Tree Char)
bRight = do _  <- char ','
            t2 <- nonemptyTreeP
            return (Empty, t2)

-- Branching in both directions.
bBoth :: Parser (Tree Char, Tree Char)
bBoth = do  t1 <- nonemptyTreeP
            _  <- char ','
            t2 <- nonemptyTreeP
            return (t1, t2)

-- Branching left.
bLeft :: Parser (Tree Char, Tree Char)
bLeft = do t1 <- nonemptyTreeP
           _  <- char ','
           return (t1, Empty)
