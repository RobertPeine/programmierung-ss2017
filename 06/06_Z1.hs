------------------------------------------------------------------------
-- Zusatzaufgabe 1
------------------------------------------------------------------------

data UTree a = UNode a [UTree a] deriving Show
data BTree a = BNil | BNode a (BTree a) (BTree a) deriving Show

t :: UTree Int
t = UNode 1 [UNode 2 [], UNode 3 [UNode 5 [], UNode 6 []], UNode 4 []]

toBinTree :: UTree a -> BTree a
toBinTree x = toBinTree' x []

toBinTree' :: UTree a -> [UTree a] -> BTree a
toBinTree' (UNode n []) [] = BNode n BNil BNil
toBinTree' (UNode n []) (x:xs) = BNode n BNil (toBinTree' x xs)
toBinTree' (UNode n (y:ys)) [] = BNode n (toBinTree' y ys) BNil
toBinTree' (UNode n (y:ys)) (x:xs) = BNode n (toBinTree' y ys) (toBinTree' x xs)
