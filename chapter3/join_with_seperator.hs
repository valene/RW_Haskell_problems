--RWHChap03/ex3-7 
interperse :: a -> [[a]] -> [a]
interperse _ [] = []
interperse _ [x] = x
interperse sv (x:xs) = x ++ [sv] ++ (interperse sv xs)
