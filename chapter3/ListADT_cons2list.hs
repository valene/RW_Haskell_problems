--RWHchap03/recursive data type to list.
data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromcons2list (Cons x (xs)) = x:fromcons2list xs
fromcons2list (Nil) = []
