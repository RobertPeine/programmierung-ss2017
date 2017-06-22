module Main where

--    i     sum
f :: Int -> Int -> Int
f x1 x2 = if x1 > 0
          then g x1 x2 x1 1
          else x2

--    i     sum     j     prod
g :: Int -> Int -> Int -> Int -> Int
g x1 x2 x3 x4 = if x3 > 1
                then g x1 x2 (x3 - 1) (x3 * x4)
                else f (x1 - 1) (x2 + x4)

main = do x1 <- readLn
          print (f x1 0)
