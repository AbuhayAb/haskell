--- file : exam.hs
--- name : abuhay abune 

import Test.QuickCheck

data MyTree = E | R | B | Node [MyTree] deriving (Show, Read)

----- part One : my quiz
win :: MyTree -> Maybe String
win a          | winG a == (-1) = Just "B"           -- change (Tree a) --> a
               | winG a == 1    = Just "R"
               | winG a == 0    = Nothing            --- add |


winG :: MyTree -> Int
winG E                     = 0
winG R                     = 1
winG B                     = -1
winG (Node a) = if (sum $ map winG a) >0 then 1
                else if (sum $ map winG a) < 0 then -1
                     else 0 



a = Node [R,B,B]                 -- remove the word let
b = Node [R]                     -- ""
c = Node [R,R]                   -- ""
d = Node [B]                     -- ""
e = Node [a,b,c,d]               -- ""



----- part two 

instance Arbitrary MyTree where
  arbitrary = sized arbitrarySizedTree

arbitrarySizedTree :: Int -> Gen MyTree 
arbitrarySizedTree m = do
  n <- choose (0, m `div` 2)
  ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
  case n of
    0 -> oneof [return B, return R]
    _ -> return (Node ts)


prop_tree :: MyTree -> Bool
prop_tree t = (win t) == win (Node [t, R, B]) 

prop_tree1 :: MyTree -> Bool 
prop_tree1 t |  res == (Just "B")      = win (Node [t, B]) == res
             |  res == (Just "R")      = win (Node [t, R]) == res
             |  otherwise              = win (Node [t,Node [B,R]])== res
             where res = win t


--main = quickCheck prop_tree >> quickCheck prop_tree1
