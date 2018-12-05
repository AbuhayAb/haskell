-- file: tar_3.hs
-- abuhay abune
-- tar 3  

import Control.Monad
import Control.Monad.Trans.Writer
import Control.Monad.State

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

--- q3: Parse a floating point number


