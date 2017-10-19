lst_ = map f lst

f x = x*(x+1)

lst = [1..10]

main = do
     print lst_

