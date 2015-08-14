
{- Exercise #5. product xs ys
   Returns a list of pairs that is the Cartesian product of xs and ys.
   Example:
   (product ['a','b','c'] [1,2])
     == [('a',1), ('a',2), ('b',1), ('b',2) ('c',1) ('c',2)]
-}

product' :: [a] -> [b] -> [(a,b)]
product' _ [] = []
product' [] _= []
product' (x:xs) (y:ys) = (x,y):(product' [x] ys)++(product' xs [y])++(product' xs ys)


crossProd :: [Int] -> [Int] -> [(Int,Int)]
crossProd xs ys | xs == [] || ys == [] = []
                | otherwise = (head xs, head ys) : crossProd (tail xs) (ys)