--- file : exam2.hs
--- name : abuhay abune 

import Data.Char
import Control.Monad (replicateM)
import Control.Monad
import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.State hiding (modify)
import Test.QuickCheck
import System.Random
import Numeric (showHex, showIntAtBase)



-- Part 1: My sol
--q1
parseBin1 :: String -> String
parseBin1 (x:xs) = do if x == '-'
                       then (x : isBin xs)               ---- (-15)
                       else isBin (x:xs)

isBin :: String -> String
isBin xs = xs

--q2                                                     ---- No change
fac n = fix (\f k m ->
  if m <= 0                                              -- change from m==0 to m <= 0
    then k                                               -- (-1)
    else (f (k*m) (m-1))) 1 n      


--q3
modify' :: (a -> a) -> State a ()                        -- forget get and put
modify' f = get >>= put.f                                -- (-4)  

-- runState modify (+1) >> modify (/4) >> modify (*2) 1



--Part 2: Fix My sol
--q1  fix
newtype Parser a    = P (String -> [(a, String)])

instance Functor Parser where
  fmap f cs     = do x <-cs
                     return $ f x

instance Applicative Parser where
  pure              = return
  (P xs) <*> (P ys) = P $ \inp -> [( f a, out) | (f,out1) <- xs inp, (a,out) <- ys out1]

instance Monad Parser where
  return x           = P $ \cs -> [(x,cs)]
  (>>=)  p f         = P $ \inp -> concatMap (\(a, s') -> doParse (f a) s') $ doParse p inp


instance Alternative Parser where
  empty     = P (\_ -> [])
  (<|>) p q = P $ \s -> case doParse p s of
                            []     -> doParse q s
                            res    -> res

doParse         :: Parser a -> String -> [(a, String)]
doParse (P p) s = p s

item :: Parser Char
item = P $ \s -> case s of
                    (x:xs) -> [(x,xs)]
                    ""     -> []

token :: Char -> Parser Char
token c = satP (==c)

satP :: (Char -> Bool) -> Parser Char
satP p = do c <- item
            if (p c) then return c else empty

chooseP :: Parser a -> Parser a -> Parser a
chooseP p1 p2 = P $ \cs -> let lhs = doParse p1 cs in
                           let rhs = doParse p2 cs in
                           lhs ++ rhs

manyP :: Parser a -> Parser [a]
manyP p = many1 p <|> many0 
         where many0  = return []
               many1 p = do x  <- p
                            xs <- manyP p
                            return $ x:xs

oneOrZero = (token '1') `chooseP` (token '0')

parseBin :: String -> (Maybe Int, String)
parseBin x = case doParse (token '-') x of
                []          -> parseBin' x 1
                [(_,xs)]    -> parseBin' xs (-1)

parseBin' :: String -> Int ->(Maybe Int,String)
parseBin' x sign = case doParse (manyP oneOrZero) x of
                      [("",xs)] -> (Nothing,xs)
                      [(y,ys)]  -> (Just num,ys)
                                   where num = sign * n
                                         n   = binaryToDecimal y

binaryToDecimal :: String -> Int
binaryToDecimal xs = foldl (+) 0 num2
                      where num2 = [ x*y | (x,y) <- zip rhs lhs ]
                            rhs  = fmap digitToInt xs
                            lhs  = reverse ([2^t | t <- [0..((length xs)-1)]])


--q3 fix
modify :: (a -> a) -> State a ()
modify f = get >>= put.f


--quickCheck 
--q1
checkParseBin :: Int -> String -> Bool
checkParseBin n sx | n < 0     = True
                   | otherwise = y == (Just n) && str == ys
                                where (y,ys) = parseBin (num ++ str)
                                      num    = showIntAtBase 2 intToDigit n ""
                                      str    = filter isAlpha sx

--q2
checkFac :: Int -> Bool                               
checkFac n = (fac n) == (product [1..n])

--q3
checkModify :: Int -> Int -> Int -> Bool
checkModify n m k = ((m + n) * k) == (snd $ runState (modify (+m) >> modify (*k)) n)

--run main for quickCheck
--main = quickCheck checkModify >> quickCheck checkParseBin >> quickCheck checkFac
