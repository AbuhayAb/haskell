--- file : exam3.hs
--- name : abuhay abune
--- grade: 93

import Data.Char
import Control.Monad (replicateM)
import Control.Monad
import Control.Monad.Trans.Writer
import Control.Applicative
import Test.QuickCheck
import System.Random
import Numeric (showHex, showIntAtBase)



-- Part 1: My sol
--q1
data MyTree = E | Node [MyTree] deriving (Show, Read)

--q1 fix

mySum :: MyTree -> Int
mySum E   = 0
mySum (Node a) = 1 + (sum $ map mySum a)
-- change from (Node (x:xs) = 1 + (f x) + (f xs))
-- (-4)

--q2

strToIntArry :: String -> [Int]
strToIntArry = fmap (\c -> if c=='[' then 1 else -1)

checkValids :: Int -> [Int] -> Bool
checkValids n [] = (n == 0)
checkValids n (x:xs) = if (n+x) < 0 then False else (checkValids (n+x) xs)

isValide :: String -> Bool
isValide xs = checkValids 0 (strToIntArry xs)
--- chnage from g.f xs
-- forget input 0
-- (-1)

--q3

trueTableShow' :: ([Bool] -> Bool) -> Int -> Int -> Writer [String] ()
trueTableShow' f n m = if ((2^n -1 )==m) then tell [unwords $ fmap show ((h n m) ++ [f (h n m)])]   ---change (n==m+1)
                                     else do              --- and do (-1)
                                          tell [unwords $ fmap show ((h n m) ++ [f (h n m)])]
                                          -- forget use unwords and show (-1)
                                          trueTableShow' f n (m+1)

-- change from Int-> [Bool] (-1)
h :: Int -> Int-> [Bool]
h n m = fmap (\s -> if s == '1' then True else False) (rhs ++ lhs)
        where lhs  = showIntAtBase 2 intToDigit m ""
              rhs  = concat (replicate (n - (length lhs)) "0")  --add (-2)

trueTable f n = mapM_ putStrLn $ execWriter $ trueTableShow' f n 0



--quickCheck q1

instance Arbitrary MyTree where
  arbitrary = sized arbitrarySizedTree

arbitrarySizedTree :: Int -> Gen MyTree
arbitrarySizedTree m = do
  n <- choose (0, m `div` 2)
  ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
  case n of
    0 -> oneof [return E]
    _ -> return (Node ts)


prop_tree :: MyTree -> Bool
prop_tree t = ((mySum t)+1) == mySum (Node [t,E])


-- quickCheck prop_tree
--quickCheck q2
isBalanced :: String -> Bool
isBalanced = bal (-1) 0
    where bal :: Int -> Int -> String -> Bool
          bal _   0      []  = True
          bal i   _      []  = False
          bal i (-1)      _  = False
          bal i   n ('[':bs) = bal (i+1) (n+1) bs
          bal i   n (']':bs) = bal (i+1) (n-1) bs

check :: String -> Bool
check str = (isBalanced str) == (isValide str)

checkBalanced :: Int -> Bool
checkBalanced n = do
             let bs = cycle "[]"
             check $ (take n bs)


-- quickCheck checkBalanced

--quickCheck q3
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn $ reverse [toStr a ++ " " ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x)

-- check if tablen == trueTable
--
--grade:
--q1: -3
--g2: -1
--q3: -3
