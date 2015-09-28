--RWHChapter4/part2-4
--Did i miss the chapter on 'either'?
--because linear learning is `so` overrated

import Data.Char

asInt_either :: [Char] -> Either String Int
asInt_either [] = Left "Empty List"
asInt_either ('-':'-':xs) = Left "more than one -ve Sign"
asInt_either ('-':xs) = case (asInt_either xs) of 
                            Left err -> Left err
                            Right (val) -> Right((-1)*val)
asInt_either xs = foldl func (Right 0) xs
  where func (Right a) b | isDigit b = Right (a*10 + (digitToInt b))
                         | otherwise = Left "Non Digit found"
        func (Left err) b = Left err
