------------------------------------------------------------------------
-- Zusatzaufgabe 1
------------------------------------------------------------------------

avg :: [Float] -> Float
avg [] = 0
avg xs = avg' 0 0 xs
  where
    avg' acc len [] = acc / len
    avg' acc len (x:xs) = avg' (acc + x) (len + 1) xs

------------------------------------------------------------------------

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x:xs) =
  let (ls, rs) = partition p xs in
    if p x
    then (x : ls, rs)
    else (ls, x : rs)

-- oder kürzer:

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f xs = (filter f xs, filter (not . f) xs)

------------------------------------------------------------------------

maxrep :: [Int] -> Int
maxrep [] = 0
maxrep (x:xs) = maxrep' xs x 1
  where
    maxrep' :: [Int] -> Int -> Int -> Int
    maxrep' [] _ n = n
    maxrep' (x:xs) y n =
      if x == y
      then maxrep' xs x (n+1)
      else max n (maxrep' xs x 1)
