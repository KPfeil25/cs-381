-- Kevin Pfeil
-- CS 381
-- Julianne Schutfort
-- 1/25/2022

import Data.List (nub)
import HW2types

--Exercise 1

-- testBag = [(0::Int, 1::Int), (1::Int, 1::Int), (2::Int, 4::Int)]
-- testBag2 = [(0::Int, 1::Int), (2::Int, 1::Int)]
-- testSubBag = [(0::Int, 1::Int), (2::Int, 3::Int), (3::Int, 1::Int)]

ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x,1)]
ins x ((val,n):xs)
    | x == val = (val,n+1):xs -- Case where the number already exists in the bag
    | otherwise = (val,n):ins x xs -- Case where we need to add the new number into the bag

del :: Eq a => a -> Bag a -> Bag a
del x [] = [] -- Cant delete anything from an empty bag
del x ((val, n):xs)
    | x == val && n > 1 = (val, n-1):xs -- Case where the element occurs more than once, decrement it
    | x == val && n == 1 = xs -- Case where the element is in the bag once, remove it
    | otherwise = (val,n):del x xs

bag :: Eq a => [a] -> Bag a
bag = foldr ins [] -- Apply insert to the whole list

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] [] = True
subbag xs [] = False -- Indicates that the left bag has something that the right bag doesnt, ergo its false
subbag [] ys = True -- Opposite of the case immediately above
subbag (x:xs) ys = (checkInList x ys) && (subbag xs ys)

-- Helper function to check if the element is in the list
checkInList x [] = False
checkInList x (y:ys)
  | fst x == fst y && snd x <= snd y = True
  | otherwise = checkInList x ys


isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((val, n):xs)
    | n == 1 = isSet xs -- If the occurences equals one, we know we can continue onwards from this
    | otherwise = False -- O/w return

size :: Bag a -> Int
size = sum.map snd

-- Exercise 2

-- g :: Graph
-- g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

-- h :: Graph
-- h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

-- testTuples :: [(Int, Int)]
-- testTuples = [(4,5), (4,3)]

-- Helper function for performing the nodes function
tuplesToList :: [(a,a)] -> [a]
tuplesToList ((a,b):xs) = a : b : tuplesToList xs
tuplesToList _ = []

nodes :: Graph -> [Node]
nodes g = norm (tuplesToList g) -- Turn all of the tuples into a list and call the provided norm function

suc :: Node -> Graph -> [Node]
suc a [] = []
suc a ((x,y):xs)
    | a == x = y: suc a xs -- Case where the node is a successor, keep it and continue on
    | otherwise = suc a xs -- Case where the node is not a successor

detach :: Node -> Graph -> Graph
detach a [] = []
detach a ((x,y):xs)
    | a == x || a == y = detach a xs -- We have found the node that is being detatched, remove it
    | otherwise = (x,y) : detach a xs -- Not being detatched, dont remove it

cyc :: Int -> Graph
cyc 0 = [] -- Base case # 1
cyc 1 = [(1,1)] -- Base case #2
cyc x = [ (x,x+1) | x <- [1..x-1]] ++ [(x,1)] -- Add cycle for all x from x until x-1, and then append (x,1)

-- Exercise 3
-- f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
-- examplePoint::Point = (4,4)
-- examplePoint2::Point = (5,5)
-- exampleCicle = Circle (5,5) 3

-- exampleShapeButItsActuallyAPoint :: Shape = Pt (1,3)

width :: Shape -> Length
width (Pt pnt) = 0 -- Points dont have width
width (Circle pnt rad) = rad * 2 -- Diameter = 2r
width (Rect pnt wid len) = wid -- Return the width parameter

bbox :: Shape -> BBox
bbox (Pt pnt) = ((pnt), (pnt)) -- Again, just the point
bbox (Circle pnt rad) = ((fst pnt - rad, snd pnt - rad), (fst pnt + rad, snd pnt + rad)) -- Find bottom left and top right
bbox (Rect pnt wid len) = ((fst pnt, snd pnt), (fst pnt + wid, snd pnt + len)) -- We are given the bottom left and we just need to add the length and width to find top right

minX :: Shape -> Number
minX (Pt pnt) = fst pnt 
minX (Circle pnt rad) = fst pnt - rad -- Subtract the radius from the center
minX (Rect pnt wid len) = fst pnt --  Given bottom left again

-- Suggested helper function
addPt :: Point -> Point -> Point
addPt pnt pnt2 = (fst pnt + fst pnt2, snd pnt + snd pnt2)

-- This function was all about getting the syntax to work. All I did
-- to move each shape was add the vector to it using the helper function
move :: Shape -> Point -> Shape
move (Pt pnt) vec = Pt (addPt pnt vec)
move (Circle pnt rad) vec = (Circle (addPt pnt vec) rad)
move (Rect (x, y) l w) (a, b) = (Rect (addPt (x, y) (a, b)) l w)