------------------------------------------------------------------------
-- Übung 1
------------------------------------------------------------------------

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

{-
fac 3
= 3 * fac 2
= 3 * (2 * fac 1)
= 3 * 2 * (1 * fac 0)
= 3 * 2 * 1 * 1
= 6
-}

------------------------------------------------------------------------

sumFacs :: Int -> Int -> Int
sumFacs n m
  | m < n = 0
  | otherwise = fac n + sumFacs (n+1) m

-- oder:

sumFacs' :: Int -> Int -> Int
sumFacs' n m
  | n <= m = fac n + sumFacs' (n+1) m
  | otherwise = 0
  
-- oder:

sumFacs'' :: Int -> Int -> Int
sumFacs'' n m = 
  if n <= m
  then fac n + sumFacs'' (n+1) m
  else 0

------------------------------------------------------------------------
-- Übung 2
------------------------------------------------------------------------

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib i = fib (i-1) + fib (i-2)

------------------------------------------------------------------------
-- Übung 3
------------------------------------------------------------------------

prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

-- x ist in diesem Fall das erste Element der Liste und xs die Restliste

------------------------------------------------------------------------

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- mit ++ können Listen zusammengeführt werden

------------------------------------------------------------------------

-- rem ist bereits definiert
-- Ausblenden möglich mit: import Prelude hiding (rem)

rem' :: Int -> [Int] -> [Int]
rem' _ [] = []
rem' n (x:xs)
  | x /= n = x : rem' n xs -- ebenso möglich: x /= n = [x] ++ rem' n xs
  | otherwise = rem' n xs
  
-- /= wird verwendet, um auf Ungleichheit zu prüfen
-- Es gilt: not (x == n) <=> x /= n
  
-- oder:

rem'' :: Int -> [Int] -> [Int]
rem'' _ [] = []
rem'' n (x:xs)
  | x == n = rem'' n xs
  | otherwise = x : rem'' n xs

------------------------------------------------------------------------

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x] = True -- ebenso möglich: (x:[]) = True
isOrd (x:y:xs)
  | x <= y = isOrd (y:xs)
  | otherwise = False

-- oder etwas kürzer:

isOrd' :: [Int] -> Bool
isOrd' [] = True
isOrd' [x] = True
isOrd' (x:y:xs) = x <= y && isOrd' (y:xs)

------------------------------------------------------------------------

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- oder:

merge' :: [Int] -> [Int] -> [Int]
merge' [] ys = ys
merge' xs [] = xs
merge' xxs@(x:xs) yys@(y:ys)
  | x < y = x : merge' xs yys
  | otherwise = y : merge' xxs ys

------------------------------------------------------------------------
-- Zusatzaufgabe 1
------------------------------------------------------------------------

fibs :: [Int]
fibs = fibs' 0
  where
    fibs' x = fib x : fibs' (x+1)

-- take 7 fibs liefert die ersten 7 Fibonacci-Zahlen

------------------------------------------------------------------------
-- Zusatzaufgabe 2
------------------------------------------------------------------------

{-
Es gibt genau einen Binärbaum mit einem Knoten. Ein Binärbaum mit n + 1
Knoten besteht aus einem Wurzelknoten und zwei Teilbäumen, deren
Knotensumme auf n = m + (n - 1 - m) kommt, wobei m die Größe des linken
Teilbaums ist. Mit der Funktion go summieren wir über alle Möglichkeiten
für m.
-}

countBinTrees :: Int -> Int
countBinTrees 0 = 0
countBinTrees 1 = 1
countBinTrees n = go (n-1)
  where
    go 0 = 0
    go m = go (m-1) + countBinTrees (n-1-m) * countBinTrees m

{-
Die alternative Lösung verwendet die Catalan-Zahlen - eine Folge aus der
Kombinatorik für die Anzahl von Binärbäumen, die vollständigen
Klammerungen von Termen etc., welche durch eine geschlossene Formel
berechnet werden können.
-}

countBinTrees' :: Int -> Int
countBinTrees' n
  | even n = 0
  | otherwise =
      let m = div (n-1) 2
      in div (fac (2*m)) (fac m * fac (m+1))

-- div (n-1) 2 <=> (n-1) `div` 2
