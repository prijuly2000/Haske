penultimate l=last(init l)


findK k l = last(take (k+1) l)


isPalindrome l =reverse l == l


duplicate xs = concat([[xs!!i,xs!!i]|i<-[0..(length xs-1)]])


ziplike xs ys = [(xs!!i,ys!!i) | i<-[0..(min (length xs) (length ys))-1]] 


splitAtIndex k l =(take k l,drop k l)


dropK k l = init(take (k+1) l) ++drop (k+1) l


slice i k l= drop i (init(take (k+1) l))


insertElem x k l=(take 5 l)++ [x] ++(drop 5 l)


rotate n l = drop n l ++ take n l
