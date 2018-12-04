-- file: tar_3.hs
-- abuhay abune
--Tar 3 :Hanoi towers, tracking the steps with the writer monad 

--A function that adds a random number every "even" time it used and subtracts a random number every "odd" time it is used (state monad) 
import Control.Monad
import Control.Monad.Trans.Writer
import Control.Monad.State

---- q1
type Peg  = String
type Move = (Peg,Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _      _      _     = []
hanoi 1 source target _     = [(source,target)]
hanoi n source target spare = hanoi (n-1) source spare target ++
                              hanoi 1 source target spare ++
                              hanoi (n-1) spare target source


calc :: Integer -> Writer [String] ()
calc n = do
  tell ["start"]
  hanoiShow $ hanoi n "a" "b" "c"

hanoiShow :: [Move] -> Writer [String] ()
hanoiShow []     = tell ["done"]
hanoiShow (x:xs) = do
  tell ["Move " ++ (fst x) ++" -> "++ (snd x)]
  hanoiShow xs

test n = mapM_ putStrLn $ execWriter $ calc n

----q2

fromStoAandS :: Int -> (String,Int)
fromStoAandS c | c `mod` 2 == 0 = ("even",c+1)
               | otherwise      = ("odd",c+1)

stateIntString :: State Int String 
stateIntString = state fromStoAandS

{-
type RandomState a = State StdGen a
rand :: IO Int
rand = getStdRandom (randomR (0,maxBound))

getRandom :: Random a => RandomState a
getRandom = get >>= \gen -> 
            let (val,gen') = random gen in
            put gen' >>
            return val



eventTime :: IO Integer -> IO Bool
eventTime t = fmap even t

--func :: Integer => IO Intger

-- Parse a floating point number

main = do test 2
          test 3
-}
