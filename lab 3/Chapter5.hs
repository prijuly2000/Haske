
power :: Int -> Int -> Int
power _ 0 = 1
power x n = x*power x (n-1)


-- Tail-recursive version
power' :: Int -> Int -> Int
power' x n = powerHelper x n 1 	where 
	powerHelper x 0 rsf = rsf
	powerHelper x n rsf = powerHelper x (n-1) (rsf*x)



fib :: (Num a, Eq a) => a -> [a]
fib 0=[0]
fib 1=[1,0]
fib n= (head(fib(n-1))+head(tail(fib(n-1)))):fib(n-1)



-- Tail-recursive version
fib' :: (Num a, Eq a) => a -> [a]
fib' n = fibHelper n [1,0] where
  fibHelper 0 rsf = [0]
  fibHelper 1 rsf = rsf
  fibHelper n rsf = fibHelper (n-1) ([head rsf + head (tail rsf)]++rsf)
    


list_set :: (Eq a)=>[a] -> Int -> a -> [a]
list_set [] n y=[]
list_set (x:xs) n y  
	| n==0 = y:(list_set xs (n-1) y)
	| otherwise = x:(list_set xs (n-1) y)
	
	

-- Tail-recursive version
list_set' :: [a] -> Int -> a -> [a]
list_set' xs n y = listHelper xs n y [] where
  listHelper [] n y rsf=rsf
  listHelper (x:xs) n y rsf 
    | n==0 = listHelper xs (n-1) y (rsf++[y])
    | otherwise = listHelper xs (n-1) y (rsf++[x])



count_occurrences :: (Eq a) => a -> [a] -> Int
count_occurrences _ []=0
count_occurrences y (x:xs) 
  | x==y =(1+count_occurrences y xs)
  | otherwise = (0+count_occurrences y xs)
  
  


-- Tail-recursive version
count_occurrences' :: (Eq a) => a -> [a] -> Int
count_occurrences' y xs= countHelper y xs 0 where
  countHelper y [] rsf=rsf  
  countHelper y (x:xs) rsf
    | y == x = countHelper y xs (1+rsf)
    | otherwise = countHelper y xs rsf




product' :: [a] -> [b] -> [(a,b)]
product' _ [] = []
product' [] _= []
product' (x:xs) (y:ys) = (x,y):(product' [x] ys)++(product' xs [y])++(product' xs ys)



-- tail-recursive
product'' :: [a] -> [b] -> [(a,b)]
product'' xs ys = productHelper xs ys [] where  
  productHelper [] ys rsf = rsf
  productHelper (x:xs) ys rsf = productHelper xs ys (rsf++[(x,y) | y<-ys])
     
  
up :: [[a]] -> [a]
up []=[]
up (x:xs) = x++up xs



-- tail-recursive
up' :: [[a]] -> [a]
up' xs = upHelper xs [] where
    upHelper [] rsf=rsf
    upHelper (x:xs) rsf=upHelper xs (rsf++x)



stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign n k
  | k==0= n*(-1)
  | n<0=stepReverseSign (n-1) (k-1)
  | otherwise=stepReverseSign (n+1) (k-1)



{- Exercise #8. piCalc err
   Lets calculate pi.
   The Leibniz formula for pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
   Can be defined as pi = (4/1) - (4/3) + (4/5) - (4/7) ....
   We can create a function, where given a certain tolerance, we can recursively calculate
   Pi to within that tolerance.
   Lets create two functions, piCalc, and piCalc', the latter we will recursively call
   until our pi calculation is within the tolerance
   The piCalc function is defined as:
   piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)
   Given a tolerance, say, 0.001, it will return a tuple.
   fst is pi to an accuracy of the tolerance, 0.001 in this case
   snd is the number of recursive steps taken to calculate it, after all this chapter is about recursion!
   Example: piCalc 0.001 = (3.1420924036835256,2000)
   The piCalc' function is defined as 
   piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
   Lots of parameters!
   The first parameter is the current denominator from the Leibniz formula
   The next is our calculation of pi from our previous attempt
   The next is the tolerance
   The final parameter is the number of times this function has been called (ie, we add one every time we recurse
   Example piCalc' 1 0.0 0.001 0 = (3.1420924036835256,2000)

   Feel free to change the parameter order, what parameters you need etc in order to get this to work for you,
   But, of course the output of piCalc should remain as (pi, count)

   You may find the stepReverseSign function handy
-}
piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)
piCalc a = undefined

piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
piCalc' w x y z = undefined

