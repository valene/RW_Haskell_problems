--RWHChap03/ex3-13
--graham scan idea :
--find the `pivot point` with lowest y and x coord - DONE
--sort points in order of angle (point, pivot and x-axis)
--take 3 points and keep if Leftside turn,
--complete the full array.
import Data.List
data Directions = Leftside | Rightside | Straight deriving (Show,Eq)

data Coords2d = Coords2d Double Double deriving (Show,Eq)

data Vector2d = Vector2d Double Double deriving (Show)

vectorize2d :: Coords2d -> Coords2d -> Vector2d
vectorize2d (Coords2d x1 y1) (Coords2d x2 y2) = Vector2d (x2 - x1) (y2 - y1)

compslope :: Vector2d -> Vector2d -> Double
compslope (Vector2d i j) (Vector2d l m) = i*m - l*j 

gendirection :: Coords2d -> Coords2d -> Coords2d -> Directions
gendirection p1 p2 p3 | compslope (vectorize2d p1 p2) (vectorize2d p2 p3) > 0 = Leftside
                      | compslope (vectorize2d p1 p2) (vectorize2d p2 p3) < 0 = Rightside
                      | otherwise = Straight

gendirectlist :: [Coords2d] -> [Directions]
gendirectlist xs | length xs < 3 = []
gendirectlist (c1:c2:c3:xss) = (gendirection c1 c2 c3): gendirectlist(c2:c3:xss)

comparec (Coords2d x1 y1) (Coords2d x2 y2) | y1 == y2 = compare (x1) (x2)
                                           | otherwise = compare (y1) (y2)

getcos :: Vector2d -> Double
getcos (Vector2d i j) = i / sqrt((i*i) + (j*j))

sortbypivot :: [Coords2d] -> [Coords2d]
sortbypivot xs = sortBy (\c1 c2 -> comparec c1 c2) xs

sortbyangle :: [Coords2d] -> [Coords2d]
sortbyangle (x:xs) = x : sortBy (\xs1 xs2 -> flip compare (getcos $ vectorize2d x xs1) (getcos $ vectorize2d x xs2)) xs

fulltraverse :: [Coords2d] -> Int -> [Coords2d]
fulltraverse xs n | length(xs) == n+1 && gendirection (xs !! (n-1)) (xs !! n) (xs !! 0) == Rightside = delete (xs !! n) xs
                  | length(xs) == n+1 && gendirection (xs !! (n-1)) (xs !! n) (xs !! 0) == Leftside = xs
                  | gendirection (xs !! (n-1)) (xs !! n) (xs !! (n+1)) == Rightside = fulltraverse (delete (xs !! n) xs) n
                  | otherwise = fulltraverse xs (n+1)

getconvexset :: [Coords2d] -> [Coords2d]
getconvexset [] = []
getconvexset xs | length(xs) < 3 = xs
                | otherwise = fulltraverse (sortbyangle $ sortbypivot xs) 2
