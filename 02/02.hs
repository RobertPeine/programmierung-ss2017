import Prelude hiding (words, unwords)

------------------------------------------------------------------------
-- Übung 1
------------------------------------------------------------------------

pack :: [Char] -> [[Char]]
pack [] = []
pack (x:xs) = pack' x (x:xs) : pack'' x xs

pack' :: Char -> [Char] -> [Char]
pack' _ []= []
pack' y (x:xs)
  | y == x = x : pack' y xs
  | otherwise = []

pack'' :: Char -> [Char] -> [[Char]]
pack'' _ [] = []
pack'' y (x:xs)
  | y == x = pack'' y xs
  | otherwise = pack (x:xs)

-- oder:

pack''' [] = []
pack''' (x:xs) = ys : pack''' zs
  where
    (ys, zs) = takeall x (x:xs)
    takeall _ [] = ([], [])
    takeall x (y:ys)
      | x == y = let (us, vs) = takeall x ys in (y:us, vs)
      | otherwise = ([], (y:ys))

------------------------------------------------------------------------

encode :: [Char] -> [(Int, Char)]
encode xs = f (pack xs)
  where
    f [] = []
    f ((x:xs):ys) = (length (x:xs), x) : f ys
    -- oder: f (y@(x:xs):ys) = (length y, x) : f ys
    -- oder: f (y:ys) = (length y, head y) : f ys

------------------------------------------------------------------------

decode :: [(Int, Char)] -> [Char]
decode [] = []
decode ((n, x):xs) = repeat n x ++ decode xs
  where
    repeat 0 _ = []
    repeat n x = x : repeat (n-1) x

------------------------------------------------------------------------

rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate xxs@(x:xs) n
  | n == 0 = xxs
  | n > 0 = rotate (xs ++ [x]) (n-1)
  | otherwise = rotate (last xxs : take (length xxs - 1) xxs) (n+1)

-- oder:

rotate' :: [Int] -> Int -> [Int]
rotate' [] _ = []
rotate' xxs@(x:xs) n
  | n == 0 = xxs
  | n > 0 = rotate' (xs ++ [x]) (n-1)
  | otherwise = rotate' xxs (length xxs + n)

-- Verwendung von n `mod` length xxs zur Reduzierung der Durchläufe:

rotate'' :: [Int] -> Int -> [Int]
rotate'' [] _ = []
rotate'' xxs@(x:xs) n
  | n `mod` length xxs == 0 = xxs
  | n > 0 = rotate'' (xs ++ [x]) (n `mod` length xxs - 1)
  | otherwise = rotate'' xxs (length xxs + n `mod` length xxs)

-- Es gilt: mod n (length xxs) <=> n `mod` length xxs

------------------------------------------------------------------------
-- Übung 2
------------------------------------------------------------------------

unwords :: [String] -> String
unwords [] = ""
unwords [x] = x -- ebenso möglich: (x:[]) = x
unwords (x:xs) = x  ++ ' ' : unwords xs
-- oder: unwords (x:xs) = x  ++ " " ++ unwords xs

------------------------------------------------------------------------

words :: String -> [String]
words s = f "" s
  where
    f :: String -> String -> [String]
    f ys "" = [ys]
    f ys (x:xs)
      | x /= ' ' = f (ys ++ [x]) xs
      | ys == "" = f "" xs
      | otherwise = ys : f "" xs

-- oder:

words' :: String -> [String]
words' [] = []
words' (' ':cs) = words' cs
words' cs = let (w, cs') = takeWord cs in w : words' cs'
  where
    takeWord [] = ([],[])
    takeWord (' ':cs) = ([], cs)
    takeWord (c:cs) = let (w, cs') = takeWord cs in (c:w, cs')

------------------------------------------------------------------------
-- Übung 3
------------------------------------------------------------------------

{-
max_length [[1,2,3], [4,5,6], [7,8], [9], [1,2,3,4]] = 4
max_length [[], [], []] = 0
max_length [] = -1 (Sonderbehandlung auch anders möglich)
-}

length' :: [Int] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

max' :: Int -> Int -> Int
max' x y
  | x > y = x
  | otherwise = y
  
max_length :: [[Int]] -> Int
max_length [] = -1
max_length (x:xs) = max' (length' x) (max_length xs)

------------------------------------------------------------------------
-- Zusatzaufgabe 1
------------------------------------------------------------------------

{-
Das Einfügen von Elementen am Anfang einer Liste ist mit einer Laufzeit
von O(1) sehr effizient. Das Einfügen am Ende besitzt hingegen eine
Laufzeit von O(n), wobei n die Länge der Liste ist.

Durch den Queue-Datentyp wird das schnelle Einfügen am Ende ermöglicht.
-}

type Queue = ([Int],[Int])

isEmpty :: Queue -> Bool
isEmpty ([],[]) = True
isEmpty _ = False

{-
Die Funktion queueify wird genutzt, um die Invariante
Queue (l, r) nicht leer <=> l nicht leer
zu realisieren
-}

queueify :: Queue -> Queue
queueify ([],rs) = (reverse rs, [])
queueify q = q

enqueue :: Int -> Queue -> Queue
enqueue i (ls, rs) = queueify (ls, i:rs)

first :: Queue -> Int
first ([],[]) = error "empty queue"
first (l:_,_) = l

rest :: Queue -> Queue
rest ([],[]) = error "empty queue"
rest (_:xs,ys) = queueify (xs,ys)
