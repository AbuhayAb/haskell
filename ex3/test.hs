import Control.Monad.Writer  
import Control.Monad.State 
import System.Random
    
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)



addStuff :: Int -> Int  
addStuff x = let  
   a = (*2) x  
   b = (+10) x  
   in a+b

type Stack = [Int]  
{-      
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  
      
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)  
stackManip = do  
     push 3
     a <- pop
     pop
-}


pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int 
stackManip = do
     push 3
     pop
     pop

stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5 
        then push 5  
        else do  
            push 3  
            push 8 

moreStack :: State Stack ()  
moreStack = do
    a <- stackManip
    if a == 100 
        then stackStuff  
        else return ()


stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]

randomSt :: (RandomGen g, Random a) => State g a  
randomSt = state random


threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c)


keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False
{-
execState :: State s a -> s -> s
execState p s = snd $ runState p s
-}

type RandomState a = State StdGen a
rand :: IO Int
rand = getStdRandom (randomR (0,maxBound))

getRandom :: Random a => RandomState a
getRandom = get >>= \gen -> 
            let (val,gen') = random gen in
            put gen' >>
            return val


increaseApplicationCount :: State RandomState Int
increaseApplicationCount = state $ \s -> if even s 
                                         then ((), s + 1)
                                         else ((), s + 1)
                                        

countApplications :: State Int Int
countApplications = do
    increaseApplicationCount
    increaseApplicationCount


