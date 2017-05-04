import Prelude hiding (even)

------------------------------------------------------------------------
-- Zusatzaufgabe 1
------------------------------------------------------------------------

data Tree = Node Int [Tree]

evenTree :: Tree
evenTree = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]

unevenTree :: Tree
unevenTree = Node 1 [Node 2 [], Node 3 [Node 4 [Node 6 []], Node 5 []]]

------------------------------------------------------------------------

noLeaves :: Tree -> Int
noLeaves (Node a []) = 1
noLeaves (Node a xs) = noList xs
  where
    noList :: [Tree] -> Int
    noList [] = 0
    noList (x:xs) = noLeaves x + noList xs

------------------------------------------------------------------------

even :: Tree -> Bool
even (Node a l) = length l `mod` 2 == 0 && everyEven l
  where
    everyEven :: [Tree] -> Bool
    everyEven [] = True
    everyEven (x:xs) = even x && everyEven xs
