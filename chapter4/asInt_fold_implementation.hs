--RWHChapter4/part2-1
--Alternate strategy : `any isDigit xs` to check 
--2 traversal of list : !not_ideal && code_clarity
import Data.Char

asInt_fold :: [Char] -> Int
asInt_fold [] = error "err msg"
asInt_fold ('-':'-':xs) = error "err msg 2"
asInt_fold ('-':xs) = (-1)*(asInt_fold xs)
asInt_fold xs = foldl func 0 xs
   where func a b | isDigit b = a*10 + (digitToInt b)
                  | otherwise = error "err msg 3"
