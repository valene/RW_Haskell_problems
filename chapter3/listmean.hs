--RWHChap03/ex3-3
listsum :: [Float] -> Float
listsum [] = 0.0
listsum (x:xs) = x + (listsum xs)

listmean :: [Float] -> Float
listmean [] = 0.0
listmean xs = (listsum xs) / (fromIntegral(length xs))
