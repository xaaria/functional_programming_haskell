-- OP / 2019-09-01 / language: Haskell

-- Returns the next date as tuple (y,m,d)
-- If m was 12, year +1, day and month to 01 (Jan. 01)
-- Else only month +1 and date back to 1
-- and if not last day of month, just increment day +1
-- Return: Tuple (y,m,d)
nextDate (y,m,d) = if isLastDayOfMonth (y,m,d) then 
    
    if m >= 12 then 
        (y+1, 1, 1)
    else
        (y, m+1, 1)
    
else
    (y,m,d+1)



-- Calculates [recursively] distance between two dates (no validation checking for params!)
-- Assumed that params (and they are valid dates): (y,m,d) <= (y2,m2,d2)
-- Stops rec. calls when receives end date and returns 0
-- >> Otherwise returns 1 + and continues to next date 
-- Return: Integer (>= 0)
getDist (y,m,d) (y2,m2,d2) = if ( y==y2 && m==m2 && d==d2 ) then 
    0
else
    1 + ( getDist (nextDate (y,m,d)) (y2,m2,d2) )



-- Check if given date is the last day of that month.
-- Trivial check -02-29 is always considered to be last day of month.
-- When date is 02-28, check if that year is leap year (True, NOT the last day and False otherwise...)
-- Return: Boolean
isLastDayOfMonth (y,m,d) = if ( (d==30 && m `elem` [4,6,9,11]) || (d==31 && m `elem` [1,3,5,7,8,10,12]) || (isLeap y && d==29 && m==2) ) 
    then
        True
    else if (m==2 && d==28) then 
        if isLeap y then False else True
    else
        False
        
--
    
    
-- Check if given year is leap year or not
-- Return: Boolean
isLeap y = if y `mod` 4 /= 0 
    then False
    else if mod y 100 /= 0
        then True
    else if mod y 400 /= 0 
        then False
    else True
    
    
    
-- 01-04
-- Write a function that, given a date, calculates the next date.
-- Using that function, write another function that tells the distance of two dates. 
-- The distance of today and tomorrow is 1, 
-- today and the day after tomorrow is 2, etc. Use recursively the function that calculates the next date.

-- We will assume that date is valid. This means that yyyy-02-29 is though to be the last day of that month (implicit check)
-- Only yyyy-02-29 is checked, if leap year: it is NOT the last day of that month.