-- file: tar_3.hs
-- abuhay abune 319311759
--Tar 3 :Hanoi towers, tracking the steps with the writer monad 

--A function that adds a random number every "even" time it used and subtracts a random number every "odd" time it is used (state monad) 
import Control.Monad
import Control.Monad.Trans.Writer
import Control.Monad.State

---- q1
type Peg  = String

hanoi_ :: Integer -> Peg -> Peg -> Peg -> Writer [String] ()
hanoi_ 1 source target _     = tell ["Move " ++ source ++" -> "++ target]
hanoi_ n source target spare = do
                              hanoi_ (n-1) source spare target
                              hanoi_ 1 source target spare
                              hanoi_ (n-1) spare target source


hanoi n = mapM_ putStrLn $ execWriter $ hanoi_ n "a" "b" "c"

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

main = do hanoi 2
          hanoi 3
-}
