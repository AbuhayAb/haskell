--- file: tar_1.hs
--- 

-------------------------------q1-----------------------------------------------------
---  Find n’th generation in the sequence 1 / 1 1 / 2 1 / 1 2 1 1/ 
group :: String -> [[Char]]
group []   = []
group (x:xs) = (x : takeWhile (== x) xs) :group (dropWhile (== x) xs)

say :: String -> String
say xs = (show.length) xs ++ [head xs]

f :: String -> String
f x = concat $ map say $ group x 

look_and_say :: Int -> String
look_and_say n = (iterate f "1") !! n 

-------------------------------q2------------------------------------------------------
--- Define your own list with conversions back and forth to Haskell's List

--- infixr 5 :-: = the operator(:-:) is right-associative in level 5
infixr 5 :-:
data ListADS a         = Empty | a :-: (ListADS a) deriving (Show, Read, Eq, Ord)

---- operator like ++ for list
infixr 5 .++
(.++) :: ListADS a -> ListADS a -> ListADS a 
Empty  .++ ys          = ys
(x :-: xs) .++ ys      = x :-: (xs .++ ys)

---- safe Head for listADS
listADSHead :: ListADS a -> Maybe a 
listADSHead Empty      = Nothing
listADSHead (x :-: _)  = Just x

---- safe Tail for listADS
listADSTail :: ListADS a -> Maybe a 
listADSTail Empty                = Nothing
listADSTail ( x :-: Empty)       = Just x
listADSTail (_ :-: x :-: Empty)  = Just x

---- toList: listADS to list
toList :: ListADS a -> [a] 
toList Empty             = []
toList ( x :-: xs)       = x : toList xs

---- fromList: form list to  listADS
fromList :: [a] -> ListADS a 
fromList []              = Empty
fromList ( x : xs)       = x :-: fromList xs

-------------------------------q3------------------------------------------------------
--- Implement your own elem for list in 3 different ways

myElem :: (Eq a) => a -> [a] -> Bool
myElem x xs = any (== x) xs

myElem1 :: (Eq a) => a -> [a] -> Bool
myElem1 _ [] = False
myElem1 x xs = (x == head xs) || myElem1 x (tail xs)

myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 x xs = (not.null) [y | y <- xs , y == x]


-------------------------------q4------------------------------------------------------
--- sublist from i'th to j'th element 

mySubList :: Int -> Int -> [a] -> [a]
mySubList i j xs = take (j-i) $ drop i xs


-------------------------------q5------------------------------------------------------
---- Define general rooted tree, implement elem and sum for it

data Tree a = Node (Tree a) a (Tree a) 
            | Leaf deriving (Show, Read)

--instance Foldable Tree where
--     foldr f acc Leaf = acc
--     foldr f acc (Node l x r) = foldr f (f x (foldr f acc r)) l

singleton :: a -> Tree a
singleton x = Node Leaf x Leaf

treeInsert :: (Ord a, Num a) => a -> Tree a -> Tree a
treeInsert x Leaf = singleton x
treeInsert x (Node left y right)
    | x == y       = Node left x right
    | x < y        = Node (treeInsert x left) y right
    | x > y        = Node left y (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Leaf = False
treeElem x (Node left y right)
    | x == y       = True
    | x < y        = treeElem x left
    | x > y        = treeElem x right

--treeSum :: (Num a) =>Tree a -> a
--treeSum = foldr (+) 0

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Leaf         = Leaf
treeMap f (Node l a r) = Node (treeMap f l) (f a) (treeMap f r)

treeFoldr :: (Num t1) => (t1 -> t2 -> t2) -> t2 -> Tree t1 -> t2
treeFoldr f acc Leaf         = acc
treeFoldr f acc (Node l x r) = treeFoldr f (f x (treeFoldr f acc r)) l

