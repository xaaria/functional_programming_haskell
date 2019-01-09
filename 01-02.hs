isLeap y = if y `mod` 4 /= 0 
    then False
    else if mod y 100 /= 0
        then True
    else if mod y 400 /= 0 
        then False
    else True

-- 01-02.hs
-- Task 2 :: Write a function to check if a year is a leap year.
-- https://en.wikipedia.org/wiki/Leap_year#Algorithm
