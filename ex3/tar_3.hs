-- file: tar_3.hs
-- abuhay abune
-- tar 3  

import Control.Monad 
import Control.Monad (replicateM)
import Control.Monad.Trans.Writer
import Control.Monad.State
import System.Random

---- q1: Hanoi towers, tracking the steps with the writer monad 
type Peg  = String

hanoi_ :: Integer -> Peg -> Peg -> Peg -> Writer [String] ()
hanoi_ 1 source target _     = tell ["Move " ++ source ++" -> "++ target]
hanoi_ n source target spare = do
                              hanoi_ (n-1) source spare target
                              hanoi_ 1 source target spare
                              hanoi_ (n-1) spare target source


hanoi n = mapM_ putStrLn $ execWriter $ hanoi_ n "a" "b" "c"


--- q2: A function that adds a random number every "even" time it used 
--and subtracts a random number every "odd" time it is used (state monad) 



rollDie :: State StdGen Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1, 100) generator
             put newGenerator
             return value

nRollState :: Int -> State StdGen Int
nRollState n = do rhs <- replicateM n rollDie
                  let lhs = take n $ cycle [1,-1]
                      ret = foldr (+) 0 [x*y | (x,y) <- zip rhs lhs]
                  return ret


--- q3: Parse a floating point number

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


parseFloat' :: String -> String -> (Maybe Float,String)
parseFloat' x sign = case doParse (manyP digitChar) x of
                        [("",_)]       -> (Nothing, x)
                        [(y,ys)]       -> case doParse (token '.') ys of
                                             []       -> (Just $ (read (sign ++ y) :: Float),ys)
                                             [(s,xs)] -> case doParse (manyP digitChar) xs of
                                                            [("",tx)] -> (Just (read (sign ++y ++ [s] ++ "0") :: Float), tx)
                                                            [(t,tx)]  -> (Just (read (sign ++y ++ [s] ++   t) :: Float), tx)



parseFloat :: String -> (Maybe Float,String)
parseFloat x = case doParse (token '-') x of 
                  []         -> parseFloat' x ""
                  [('-',xs)] -> parseFloat' xs "-"

