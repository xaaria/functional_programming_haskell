

-- Task 02-02
-- "Write a function that, given a list of strings xs, creates a list 
-- of all two-character strings that can be made from characters that appear 
-- in at least one of the strings in xs. E.g. if xs = ["no","way"] 
-- then the function returns ["yy","ya","yw","yo","yn","ay","aa","aw","ao","an","wy","wa","ww",
-- "wo","wn","oy","oa","ow","oo","on","ny","na","nw","no","nn"] where the order is not important."

-- Needs some fixing

-- comb (x:xs) = x
-- Toimiva versio, tosin sanoilla: comb (x:xs) = [ (a ++b) | a <- (getc [x]:xs) , b <- (getc [x]:xs)  ]
comb (x:xs) = [ [a]++[b] | a <- "noway" , b <- getc ["no","way"] ]
-- comb (x:xs) = [ [a]++[b] | a <- (getc [x]:xs) , b <- getc ["ab","cd"] ]

getc :: [String] -> String
getc [] = []
getc ([]:_) = []
getc (w:ws) = map (\x -> x) w ++ getc ws