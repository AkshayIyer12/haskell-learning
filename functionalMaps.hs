lst_ = map f lst

f x = x*(x+1)

lst = [1..10]

main = do
     print lst_
     print accl
     print accr

g = (/)
g' = (/)

accl = foldl g 1 lst

accr = foldr g' 1 lst

filtered pred lst
         | null lst = []
         | otherwise = if pred x
           then x:filtered pred xs
           else filter pred xs
             where x:xs=lst

