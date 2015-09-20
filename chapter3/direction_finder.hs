--RWHChap03/ex3-11

data Directions = Leftside | Rightside | Straight deriving (Show)

data Coords2d = Coords2d Double Double deriving (Show)

data Vector2d = Vector2d Double Double deriving (Show)

vectorize2d :: Coords2d -> Coords2d -> Vector2d
vectorize2d (Coords2d x1 y1) (Coords2d x2 y2) = Vector2d (x2 - x1) (y2 - y1)

--blow up for y-axis
getslope :: Vector2d -> Double
getslope (Vector2d i j) = j/i

compslope :: Vector2d -> Vector2d -> Double
compslope (Vector2d i j) (Vector2d l m) = i*m - l*j 

gendirection :: Coords2d -> Coords2d -> Coords2d -> Directions
gendirection p1 p2 p3 | compslope (vectorize2d p1 p2) (vectorize2d p2 p3) > 0 = Leftside
                      | compslope (vectorize2d p1 p2) (vectorize2d p2 p3) < 0 = Rightside
                      | otherwise = Straight

