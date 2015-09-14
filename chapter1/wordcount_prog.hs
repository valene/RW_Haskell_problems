--RWChapter1/ex1-3
main = interact wordCount
  where wordCount input = show (length $ words input) ++ "\n"
