--RWHChap03/Tree constructor with maybe
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
            | Empty
              deriving (Show)

sampleTree = Node "parent" (Just (Node "left child" Nothing Nothing))
                           (Just (Node "right child" Nothing Nothing))
