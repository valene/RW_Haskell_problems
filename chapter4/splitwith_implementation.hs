--RWHChapter4/ex4-2.hs
splitWith :: (a->Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs | all f xs = [[]]
splitWith f xs | not (any f xs) = [xs]
splitWith f xs = (l:(splitWith f n))
 where (l,m:n) = break f xs
