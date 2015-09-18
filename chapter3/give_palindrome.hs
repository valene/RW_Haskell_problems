--RWHChap03/ex3-4
genpalindrome :: [a] -> [a]
--genpalindrome [] = []
--genpalindrome (x:xs) = x:(genpalindrome xs) ++ [x] 
genpalindrome xs
        | length(xs) <= 0  = []
        | otherwise = (head xs):(genpalindrome (tail xs)) ++ [head xs]
