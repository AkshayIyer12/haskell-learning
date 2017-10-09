lengthh :: [a] -> Int
lengthh [] = 0
lengthh (x:xs) = 1 + lengthh xs

filtter :: (a->Bool) -> [a] -> [a]
filtter pred [] = []
filtter pred (x:xs)
  | pred x      = x : filtter pred xs
  | otherwise   = filtter pred xs

(.) :: (b->c) -> (a->b) -> a -> c
(f . g) x = f (g x)
dd = foldr (:) [] [1,2,3]
