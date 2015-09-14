--RWHChapter1/ex1-4.hs
main = interact wordCount
  where wordCount input = show (length input) ++ "\n"
