------------------------------------------------------------------------
-- Übung 4
------------------------------------------------------------------------

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

testTree :: Tree Int
testTree = Node 1 (Leaf 2) (Leaf 3)

------------------------------------------------------------------------

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf a) = Leaf (f a)
tmap f (Node a l r) = Node (f a) (tmap f l) (tmap f r)
