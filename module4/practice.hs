-- | 

data Pet = Cat | Dog | Fish | Parrot String

hello :: Pet -> String
hello x =
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name

-- find max in an Int list
maxhelper :: Int -> [Int] -> Int
maxhelper x [] = x
maxhelper x (y:ys)  = maxhelper
  (if x>y then x else y) ys

maxfromlist :: [Int] -> Maybe Int
maxfromlist [] = Nothing
maxfromlist (x:xs) = Just (maxhelper x xs)



