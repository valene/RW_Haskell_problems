--RWHChap03/ex3-6
import Data.List
--compare' xs1 xs2 = compare (length xs1) (length xs2)

lengthsortoflists :: [[a]] -> [[a]]
lengthsortoflists xs = sortBy (\a b -> compare (length a) (length b)) xs
