--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n _ | n <= 0 = []
mytake _ [] = []
mytake n (x:xs) = x : mytake (n - 1) xs
--- ### mydrop

-- don't forget to put the type declaration or you will lose points!

mydrop :: Int -> [a] -> [a]
mydrop n (x:xs) | n <= 0 = x:xs
mydrop _ [] = []
mydrop n (x:xs) = mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = aux (x:xs) []
    where
        aux [] acc = acc
        aux (y:ys) acc = aux ys (y:acc)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] y = y
app (x:xs) y = x : app xs y

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y):myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs a b = aux (myzip a b)
    where
        aux [] = []
        aux ((x,y):xs) =(x+y):aux xs

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1:ones
--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
-- credits: https://wiki.haskell.org/The_Fibonacci_sequence
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add y [] = [y]
add y (x:xs) 
    | x == y = (x:xs)
    | x > y = y:x:xs
    | otherwise = x:add y xs


--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a] 
union x [] = x
union [] y = y
union (x:xs) (y:ys) 
    | x == y = x:union xs ys
    | x < y  = x:union xs (y:ys)
    | otherwise = y:union (x:xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a] 
intersect x [] = []
intersect [] y = []
intersect (x:xs) (y:ys) 
    | x == y = x:intersect xs ys
    | x < y  = intersect xs (y:ys)
    | otherwise = intersect (x:xs) ys
--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]] 
powerset [] = [[]]
powerset (x:xs) = union (helper x ps) ps 
    where 
        ps = powerset xs
        helper z [] = []
        helper z (y:ys) = (add z y) : (helper z ys)

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' = P.map (+1) 

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = P.foldl (+) 0
