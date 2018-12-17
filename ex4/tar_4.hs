--file: tar_4.hs
-- abuhay abune

--q1: Use “fix” to compute addition using only
-- “succ”, "pred" and  "test for zero" as primitives
import Control.Monad.Fix

plus :: Int -> Int -> Int 
plus n 0 = n
plus n m = plus (succ n) (pred m)



plus' x y = fix (\f n m -> if m==0 then n else (f (succ n) (pred m))) x y

