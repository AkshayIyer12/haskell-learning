--Recursive definition of length function
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs
--Recursive definition of filter function
filt :: (a -> Bool) -> [a] -> [a]
filt pred []    = []
filt pred (x:xs)
  | pred x      = x : filt pred xs
  | otherwise   = filt pred xs
--Recursive definition of map function
mapArr :: (a -> b) -> [a] -> [b]
mapArr _ [] = []
mapArr f (x:xs) = f x : map f xs
--Recursive definition of foldl function
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z [] = z
                lgo z (x:xs) = lgo (f z x) xs

data Tree = Leaf | Node Int Tree Tree deriving Show
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
  let leftSorted  = isSortedTree leftSubtree minVal x
      rightSorted = isSortedTree rightSubtree x maxVal
  in x >= minVal && x < maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2)
