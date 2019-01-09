isValidDate (y,m,d) = if (d >= 1 && (d<=30 && m `elem` [4,6,9,11] || d == 31 && m `elem` [1,3,5,7,8,10,12]) ) then True
        
        else if (d == 29 && m==2) then
            isLeap y
        else 
            False
    
isLeap y = if y `mod` 4 /= 0 
    then False
    else if mod y 100 /= 0
        then True
    else if mod y 400 /= 0 
        then False
    else True


-- 01-03.hs
-- Write a function to check if a given date (y,m,d) is correct.
-- Leap Year check included

-- Check if given day is >= 1 AND: it is valid 30-day month OR valid 31-day month.
-- if not (basically a leap day ) check if that year is a leap year and return that result.
-- Otherwise False