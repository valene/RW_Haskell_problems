--RWHChap03/ex3-9 :: example of a depth-search
--using a built in extrema functions might be better than-if-else
-- or better define a max_of_two function 
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeheight :: Tree a -> Int
treeheight Empty = 0
treeheight (Node _ l Empty) = (treeheight l) + 1
treeheight (Node _ Empty r) = (treeheight r) + 1
treeheight (Node _ a b) = if (treeheight a) > (treeheight b)
                          then (treeheight a) + 1
                          else (treeheight b) + 1
