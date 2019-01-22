module Date (
    Date(..),
    allDays,
    isLeap,
    isValid,
    nextDate
) where

-- I assume this is what was asked...?  ^^
-- "-- and make a module (or modules) from the functions.."

-- 03-01.hs

-- Define our Date typeclass
data Date = Date {
    y :: Int,
    m :: Int,
    d :: Int
} deriving(Eq, Show)



-- And its functions

-- 01-01:
-- Does not check year validity (0 not valid)
allDays :: Date -> [Date]
allDays (Date y _ _) = [ (Date y m d) | m <- [1..12], d <- [1..31], (d<=30 && m `elem` [4,6,9,11]) || (d<=28 || (d==29 && isLeap (Date y 0 0)) && m==2) || (d<=31 && m `elem` [1,3,5,7,8,10,12]) ]

happyDays' (Date 1974 1 15) = "https://www.youtube.com/watch?v=slvGKU7HF6M"

-- 01-04:
-- assuming date1 is same or before date2
getDist :: Date -> Date -> Int
getDist (Date y m d) (Date y2 m2 d2) = if ( y==y2 && m==m2 && d==d2 ) then 
    0
else
    1 + ( getDist (nextDate (Date y m d)) (Date y2 m2 d2) )



-- (Date y m d) is a 'type constructor'
-- Does not check validity for days or months. Only year is checked
isLeap :: Date -> Bool
isLeap (Date 0 _ _) = error "Year 0 does not exist! LOL!"
isLeap (Date y _ _) = if y `mod` 4 /= 0 
    then False
    else if mod y 100 /= 0
        then True
    else if mod y 400 /= 0 
        then False
    else True

    
    

isValid :: Date -> Bool
isValid (Date 0 _ _) = error "Not valid! Year 0 does not exist."
isValid (Date y m d) = if (d >= 1 && (d<=28 && m==2  || d <=30 && m `elem` [4,6,9,11] || d <= 31 && m `elem` [1,3,5,7,8,10,12]) ) then True
else if (d == 29 && m==2) then
    isLeap (Date y 0 0) -- 0 0 because only year matters
else 
    False
    
    
-- Check validity first
nextDate :: Date -> Date
nextDate (Date y m d) = if isValid (Date y m d) then
    if isLastDayOfMonth (Date y m d) then 
        if m >= 12 then 
            (Date (y+1) 1 1)
        else
            (Date y (m+1) 1)
    else
        (Date y m (d+1)) -- not the last day of month. Just increment day by 1
else
    error "Invalid date"
    

    
isLastDayOfMonth :: Date -> Bool
isLastDayOfMonth (Date y m d) = if ( 
    (d==30 && m `elem` [4,6,9,11]) || 
    (d==31 && m `elem` [1,3,5,7,8,10,12]) || 
    (isLeap (Date y 0 0) && d==29 && m==2) ||  
    (not (isLeap (Date y 0 0)) && d==28 && m==2) ) then
    True -- Diven date IS last day of that month!
else
    False