f1 x1 x2 = f2 x1 1

f2 x1 x2 = if x1 > 0
           then f21 x1 x2
           else f3 x1 x2

f21 x1 x2 = f211 x1 x2

f211 x1 x2 = f212 x1 (x2 * x1)

f212 x1 x2 = f2 (x1 - 1) x2

f3 x1 x2 = x2

main = do x1 <- readLn
          print (f1 x1 0)
