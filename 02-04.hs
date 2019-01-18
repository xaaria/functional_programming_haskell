import Data.List as DL
{-
    Task 02-04

    The support of a string x in the list of strings xs is the number of strings 
    in xs where x is a substring.
    Given a list of strings xs, and integer k, find  k such two-character strings 
    that have the biggest support in xs (there can be many equal answers). 
    Return the answer as a list of (support,string) pairs.
    E.g. if xs = ["hello","there","all"] and k = 3, 
    then one possible answer is [(2,"ll"),(2,"he"),(1,"th")]
-}


-- take k (DL.sortBy (\(a,_) (b,_) -> compare a b) )
flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

-- support :: [String] -> Int -> [(String, String)]
support ws k
    | ws == [] = error "..."
    | otherwise = [ (supp words subss, subss) | words<- [ws], subss <- (flatten (map (\wx -> getsubs wx 0 k) ws))  ]




{- 
    Gets all possible substring from a single string (duplicates included).
    example: getsubs "kissa" 0 2 --> ["ki","is","ss","sa"]
-}
getsubs :: String -> Int -> Int -> [String]
getsubs w i len
    | (i+len) > length w = []
    | otherwise = [sub_ w i (i+len-1)] ++ getsubs w (i+1) len


{- 
    Gets a substring.
    params:
    w = string
    i = start index [0, len-1]
    l = stop index (0-based, included). Error if overflow!
-}
sub_ :: String -> Int -> Int -> String
sub_ w i l 
    | l > length w = error "l > length"
    | i>l = ""
    | otherwise = [w!!i] ++ sub_ w (i+1) l


{- 
    Returns Int how many substrs was found from a list of words.
    Example input: ["hello","there","all"], "ll":
    gets num. of occurences from 1. word (see func. subs), calls recursively itself
    for rest of the words
    Above example input:
    1 + 0 + 1 = 2
-} 
supp :: [String] -> String -> Int
supp [] _ = 0
supp (w:[]) s = (subs w s 0)
supp (w:ws) s = (subs w s 0) + (supp ws s)


{- 
    Take ONE string and substr and return number of substring occurences
-}
subs :: String -> String -> Int -> Int
subs w s i
    | length s == 0 = 0
    | length w == 0 = 0
    | i > (length w - length s) = 0
    | otherwise = ( if ([w!!i] ++ [w!!(i+1)] == s) then 1 else 0) + subs w s (i+1)
