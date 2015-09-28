--RWHChapter4/ex4-4.hs
--LOL....poster child problem of haskell
import Data.List
simpletranspose :: String -> String
simpletranspose xs = unlines $ transpose $ lines xs 
