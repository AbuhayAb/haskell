--file: ParserLearn.hs
--abuhay abune
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module ParserLearn (Parser
                   ,doParse
                   ,parse
                   ,manyP
                   ,parseFloat
                   ,token
                   ,parseBin
                   --,plus
                   --,parseInt
                   ) where
import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a    = P (String -> [(a, String)])

instance Functor Parser where
  fmap f cs     = do x <-cs
                     return $ f x
--  fmap f (P cs)     = P $ \inp -> [ (f a ,b) | (a,b) <- cs inp]

instance Applicative Parser where
  pure              = return
  (P xs) <*> (P ys) = P $ \inp -> [( f a, out) | (f,out1) <- xs inp, (a,out) <- ys out1]

instance Monad Parser where
  return            = returnP
  (>>=)             = bindP

instance MonadPlus Parser where
  mzero             = failP
  mplus             = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = P (\s -> doParse p s ++ doParse q s)

option :: Parser a -> Parser a -> Parser a
option  p q = P $ \s ->
  case doParse p s of
    []     -> doParse q s
    res    -> res

-- apply a parser
doParse         :: Parser a -> String -> [(a, String)]
doParse (P p) s = p s

parse :: Parser a -> String -> a
--parse m s = head [ x | (x,y) <- doParse m s , y == ""]
parse m s = case doParse m s of 
              [(res,[])] -> res
              [(_, rs)]  -> error "Parser did not consume entire stream"
              _          -> error "Parser error"

item :: Parser Char
item = P $ \s -> case s of 
                    (x:xs) -> [(x,xs)]
                    ""     -> []

item2 :: Parser Char
item2 = P f
  where f []     = []
        f (x:xs) = [(x,xs)]

returnP :: a -> Parser a
returnP x = P $ \cs -> [(x,cs)]

bindP          :: Parser a -> (a -> Parser b) -> Parser b
bindP p f = P $ \inp -> concatMap (\(a, s') -> doParse (f a) s') $ doParse p inp

 
pairP       :: Parser a -> Parser b -> Parser (a,b)
pairP p1 p2 = do x <- p1
                 y <- p2
                 return (x,y)


failP :: Parser a
failP = P (\_ -> [])



token :: Char -> Parser Char
token c = satP (==c)

satP :: (Char -> Bool) -> Parser Char
satP p = do c <- item 
            if (p c) then return c else failP


alphaChar:: Parser Char
alphaChar = satP isAlpha

digitChar :: Parser Char
digitChar = satP isDigit


digitInt :: Parser Int
digitInt =  digitChar >>= \c -> return $ ord c - ord '0'
--digitInt = do c <- digitChar
--            return $ ord c - ord '0'


chooseP :: Parser a -> Parser a -> Parser a
chooseP p1 p2 = P $ \cs -> let lhs = doParse p1 cs in
                           let rhs = doParse p2 cs in
                           lhs ++ rhs
alphaNumChar = alphaChar `chooseP` digitChar

grabn :: Int -> Parser String
grabn n | n <= 0    = return ""
        | otherwise = do c  <- item
                         cs <- grabn (n-1)
                         return $ c:cs

grab2or4 = grabn 2 `chooseP` grabn 4

intOp :: Parser (Int -> Int -> Int)
intOp = plus `chooseP` minus `chooseP` times `chooseP` divide 
  where plus   = token '+' >> return (+)
        minus  = token '-' >> return (-)
        times  = token '*' >> return (*)
        divide = token '/' >> return div


string :: [ Char ] -> Parser [ Char ]
string = mapM token


mult10  :: Parser [Int]
mult10 = do ds <- manyP digitInt
            z  <- digitInt              
            if z == 0 then return (ds ++ [z])
                      else failP             
{-
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P (\cs -> case (doParse (p1 `chooseP` p2) cs) of
                        [] -> []
                        (x:xs) -> [x])                
-}
manyP :: Parser a -> Parser [a]
manyP p = many1 p <|> many0 where
  many0  = return []

many1 p = do x  <- p
             xs <- manyP p
             return $ x:xs


oneNat :: Parser Int
oneNat = read `liftM` manyP digitChar

calc ::  Parser Int
calc = binExp <|> oneNat 
  where binExp = do x <- oneNat
                    o <- intOp                      
                    y <- calc
                    return $ x `o` y


addOp :: Parser (Int -> Int -> Int)
addOp = plus `chooseP` minus 
  where plus   = token '+' >> return (+)
        minus  = token '-' >> return (-)

mulOp :: Parser (Int -> Int -> Int)
mulOp = times `chooseP` divide 
  where times  = token '*' >> return (*)
        divide = token '/' >> return div


chainl1 :: Parser b -> Parser (b -> b -> b) -> Parser b
chainl1 p pop   = p >>= rest
   where rest x = next x <|> return x
         next x = do o <- pop
                     y <- p
                     rest $ x `o` y


parenP :: Char -> Parser b -> Char -> Parser b
parenP l p r = do token l
                  x <- p
                  token r
                  return x


sumE    = prodE `chainl1` addOp
prodE   = factorE `chainl1` mulOp
factorE = parenP '(' sumE ')' <|> oneNat


match :: String -> Parser String
match []       = return []
match (x:xs)   = do y  <- token x;
                    ys <- match xs;
                    return $ y:ys

-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p  =  plus p `mplus` return []
-- match one or more occurrences

plus :: Parser a -> Parser [a]
plus p  = do { x <- p;
               xs <- star p;
               return (x:xs) }



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


oneOrZero = (token '1' >>= \_ -> return 1) `chooseP` (token '0' >>= \_ -> return 0)

parseBin' :: String -> Int ->(Maybe Int,String)
parseBin' x sign = case doParse (manyP oneOrZero) x of
                      [([],_)] -> (Nothing,x)
                      [(y,ys)]  -> (Just num,ys)
                                   where num = sign * n
                                         n   = getNum y

parseBin :: String -> (Maybe Int, String)
parseBin x = case doParse (token '-') x of
                []          -> parseBin' x 1
                [(_,xs)]    -> parseBin' xs (-1)


getNum :: [Int] -> Int
getNum []     = 0
getNum (x:xs)= x * 2^(length xs) + getNum xs

