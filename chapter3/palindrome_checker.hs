--RWHChap03/palindrome check
checkpalindrome :: (Eq a) =>  [a] -> Bool
checkpalindrome xs 
      | length(xs) <= 0  = True
      | (head xs) /= (last xs) = False
      | otherwise = checkpalindrome (init $ tail xs)
                     
                     

                        
