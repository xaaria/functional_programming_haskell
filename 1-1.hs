dates year = [ (year,m,d) | m <- [1..12], d <- [1..31], (d<=30 && m `elem` [4,6,9,11]) || (d<=28 && m==2) || (d<=31 && m `elem` [1,3,5,7,8,10,12]) ]

-- 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
-- 01-01 Prints dates of year 2019 as tuples (y,m,d) (leap year calculation not included)
-- check: add 'lenght' at the beginning of the function, output is 365

-- :l 1-1.hs
-- dates year (example >dates 2019)