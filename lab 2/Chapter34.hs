--Chapter 3
data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    deriving (Eq, Ord, Show, Bounded, Enum)

firstColour=minBound::Colour

reverseColourOrder = reverse [minBound..maxBound]::[Colour]


findColourIndex c=length ([minBound..c]::[Colour])
paintMix c1 c2 
    |odd ((findColourIndex c1)+(findColourIndex c2)) = ([minBound..maxBound]::[Colour])!!(quot ((findColourIndex c1)+(findColourIndex c2)) 2)
    |otherwise =([minBound..maxBound]::[Colour])!!(quot ((findColourIndex c1)+(findColourIndex c2)-2) 2) 
    
    

--Chapter 4
englishDigit::Int->String
englishDigit x
	| x==0 ="Zero"
	| x==1 ="One"
	| x==2 ="Two"
  | x==3 ="Three"
	| x==4 ="Four"
	| x==5 ="Five"
	| x==6 ="Six"
	| x==7 ="Seven"
	| x==8 ="Eight"
	| x==9 ="Nine"
	| otherwise ="Unknown"
  
divTuple :: (Eq a,Fractional a) => (a,a) -> a
divTuple (_,0) = undefined
divTuple (x,y) = x/y

threeZeroList :: [Int]->Bool
{-
threeZeroList xs
  | xs!!0==0 && xs!!1==0 && xs!!2==0 = True
  | otherwise = False
-}


threeZeroList (0:0:0:xs)= True
threeZeroList (_:_:_:_)=False

