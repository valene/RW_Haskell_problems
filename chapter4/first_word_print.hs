--RWHChapter04/ex4-3.hs
--commandline manipulation

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction 
  where mainWith function = do
        args <- getArgs
        case args of 
            [input,output] -> interactWith function input output
            _-> putStrLn "error : exactly two arguments needed" 

myFunction = fwprint

fwget :: String -> String
fwget [] = ""
fwget inp = head (words inp)

fwprint :: String -> String
fwprint [] = []
fwprint xs = unlines (map fwget (lines xs))
