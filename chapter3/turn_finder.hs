--RWHChap03/ex3-12

data Directions = Leftside | Rightside | Straight deriving (Show)

data Coords2d = Coords2d Double Double deriving (Show)

data Vector2d = Vector2d Double Double deriving (Show)

vectorize2d :: Coords2d -> Coords2d -> Vector2d
vectorize2d (Coords2d x1 y1) (Coords2d x2 y2) = Vector2d (x2 - x1) (y2 - y1)

--just for novelty , 
getslope :: Vector2d -> Double
getslope (Vector2d i j) = j/i

compslope :: Vector2d -> Vector2d -> Double
compslope (Vector2d i j) (Vector2d l m) = i*m - l*j 

gendirection :: Coords2d -> Coords2d -> Coords2d -> Directions
gendirection p1 p2 p3 | compslope (vectorize2d p1 p2) (vectorize2d p2 p3) > 0 = Leftside
                      | compslope (vectorize2d p1 p2) (vectorize2d p2 p3) < 0 = Rightside
                      | otherwise = Straight

gendirectlist :: [Coords2d] -> [Directions]
gendirectlist xs | length xs < 3 = []
gendirectlist (c1:c2:c3:xss) = (gendirection c1 c2 c3): gendirectlist(c2:c3:xss)
