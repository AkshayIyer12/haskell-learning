import Data.Map
roots a b c =
  let
    det2 = b*b-4*a*c;
    det = sqrt(det2);
    rootp = (-b + det)/a/2;
    rootm = (-b - det)/a/2;
  in
    [rootm, rootp]

madx x y =
  if x > y
     then x
     else y

midx x y =
  if x < y
     then x
     else y

data Color = Red | Blue | Yellow

action color = case color of
  Red -> "action1"
  Blue -> "action2"
  Yellow -> "action3"

set :: Data.Map.Map String Integer
set = Data.Map.empty
set' = Data.Map.insert "Answer" 42 set
f = \x -> x + 1
  
g x = x + 1

add3Nums = \x y z -> x + y + z

myList = [2,3,4,5]
answer = 42
yourList = [7, answer+1, 7*8]
newList = [23, 29] ++ [ 48, 99, 56]
sequenced = [0..5] ++ [100,102..110]
seqAlpha = ['a'..'z']
multThree = [3*x | x <- [1..10]]
eve = [2*x | x <- [0..10]]
odde = [2*x + 1 | x <- [0..10]]
crossSet = [[a,b] | a <- [10,11,12] , b <- [20,21]]
retIndex = [5,3,8,7] !! 2
retIndex1 = [0..100] !! 85
retIndex2 = ['a'..'z'] !! 0
xs = [1,2,xs2 !! 5,4]
xs2 = xs ++ xs
xs3 = xs2 !! 2
xs4 = xs !! 2
dsd = zipWith (\x->(\y->(x,y))) [1,2,3] "abc"
zipss = zip [42] [True]

let greet() = do
      planet <- getLine
      home <- getLine
      putStrLn ("greetings " ++ planet ++ "ling.")
      putStrLn ("I am from " ++ home ++ ".")
      putStrLn "Take me to your leader."
