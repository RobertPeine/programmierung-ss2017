------------------------------------------------------------------------
-- Übung 1
------------------------------------------------------------------------

data Tree = Leaf Int | Branch Tree Tree deriving Show

testTree :: Tree
testTree =
  Branch
    (Branch
      (Branch
        (Leaf 1)
        (Leaf 2)
      )
      (Leaf 3)
    )
    (Branch
      (Leaf 4)
      (Leaf 5)
    )

------------------------------------------------------------------------

count_leaves :: Tree -> Int
count_leaves (Leaf _) = 1
count_leaves (Branch l r) = count_leaves l + count_leaves r

------------------------------------------------------------------------

toList :: Tree -> [Int]
toList (Leaf x) = [x]
toList (Branch l r) = toList l ++ toList r
