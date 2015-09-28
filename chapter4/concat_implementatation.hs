--RWHChapter04/part2-5

concat_sample :: [[a]] -> [a]
concat_sample xs = foldr (++) [] xs
