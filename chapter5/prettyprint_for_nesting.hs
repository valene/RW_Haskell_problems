--RWHChapter5/ex2  
--A CONCRETE EXAMPLE MIGHT HAVE BEEN HELPFULL
--Idea :: { || [ && nestnumber + 1 
--        } || ] && nestnumber -1
-- {,[,},] need to appear on seperate line && `redefine` ntabs = '\n' ++ Text ... etc 
--                                         && `modify` Char '{' <> Line <> nestfunc .. etc 
nesting :: Int -> Doc -> Doc
nesting val x = nestfunc val 0 [x]

--below , pattern_matching_fail(may) if Doc is not *sufficiently* exposed 
--possible alternative, case ` ` of 
nestfunc :: Int -> Int -> Doc -> Doc 
nestfunc _ _ [] = Empty
nestfunc nval nn (x:xs) | x == Empty = nestfunc nval nn xs
                        | x == Char '{' = ntabs nval nn <> Char '{' <> nestfunc nval (nn+1) xs
                        | x == Char '[' = ntabs nval nn <> Char '[' <> nestfunc nval (nn+1) xs
                        | x == Char '}' = ntabs nval nn <> Char '}' <> nestfunc nval (nn-1) xs
                        | x == Char ']' = ntabs nval nn <> Chat ']' <> nestfunc nval (nn-1) xs
                        | x == Char c = Char c <> nestfunc nval nn xs
                        | x == Text c = Text c <> nestfunc nval nn xs
                        | x == Line = Line <> nestfunc nval nn xs
                        | x == y `Concat` z = nestfunc nval nn (y:z:xs)
                        | x == y `Union` z = nestfunc nval nn (y:xs) `Union` nestfunc nval nn (z:xs) -- Might need to be carefull here
                                                                                                     -- Functor possibly? 
                        where 
                          ntabs x y = Text (relicate (x*y) ' ') 
