penultimate l=last(init l)
--main=print(penultimate [1,2,3,4,5])

findK k l = last(take (k+1) l)
--main=print(findK 2 [0,0,1,0,0,0])

isPalindrome l =reverse l == l
--main = print(isPalindrome "racecar")

duplicate xs = concat([[xs!!i,xs!!i]|i<-[0..(length xs-1)]])
--main=print(duplicate [1,2,3])

ziplike xs ys = [(xs!!i,ys!!i) | i<-[0..(min (length xs) (length ys))-1]] 
--main=print(ziplike [1,2,3] ["a","b","c"])

dropK k l = init(take (k+1) l) ++drop (k+1) l
--main=print(dropK 3 [0,0,0,1,0,0,0])

slice i k l= drop i (init(take (k+1) l))
--main=print(slice 3 6 [0,0,0,1,2,3,0,0,0])

insertElem x k l=(take 5 l)++ [x] ++(drop 5 l)
--main=print(insertElem 2 5 [0,0,0,0,0,0])

rotate n l = drop n l ++ take n l
--main=print(rotate 2 [1,2,3,4,5])