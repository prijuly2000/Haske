square x=x*x

area x y=x*y

circleArea r = pi * square r

isBigNumber n = n > 1000

neck itemList = if(tail(itemList)==[]) then error ("No second element in the list") else head(tail(itemList))

main = print (neck ["Yo!", "Hello, World!", "Howdy!"])