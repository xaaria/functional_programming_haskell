-- Haskell 02-01
-- filterpoints receives a distance function [that receives two points and outputs a Float], 
-- point (x,y), distance d and list of points as a param.
-- Points too far are filtered out. 
-- mandist has 

-- fn: distance function
filterpoints :: ((Float,Float) -> (Float,Float) -> Float) -> (Float, Float) -> Float -> [(Float, Float)] -> [(Float, Float)]
filterpoints fn p d ps  
    | d < 0 = error "Distance under zero! (param d)"
    | otherwise = [ (x,y) | (x,y) <- ps, fn p (x,y) <= d ]

-- Euclidean Distance d = sqrt( (y2-y1)^2 + (x2-x1)^2 )
eucdist :: (Float,Float) -> (Float,Float) -> Float
eucdist (x1,y1) (x2,y2) = 
    let ydiff = y2-y1
        xdiff = x2-x1
    in sqrt (ydiff*ydiff+xdiff*xdiff)

-- Manhattan Distance d = |x2-x1| + |y2-y1|
-- mandist is filterpoints where fitst param (dist func.) is replaced with a lambda
-- that takes point p and tuples.
-- Example: >mandist (0,0) 2 [(0,0),(3,0)]

-- "use partial evaluation to create another function, 
-- where f has been fixed to the Manhattan distance function, by using partial evaluation. 
-- Do that by fixing f with a lambda funciton. "

mandist = filterpoints (\(px,py) (x,y) -> abs (x-px) + abs (y-py))
-- hopefully this is what was asked...?










    
-- for testing
dist_test :: (Float,Float) -> (Float,Float) -> Float
dist_test (_,_) (_,_) = 9.0