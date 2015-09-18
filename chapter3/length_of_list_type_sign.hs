--RWHChap03/ex3-1
counter :: [a] -> Int
counter [] = 0
counter (x:xs) = 1+counter(xs) 
