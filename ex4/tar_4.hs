--file: tar_4.hs
-- abuhay abune

--q1: Use “fix” to compute addition using only
-- “succ”, "pred" and  "test for zero" as primitives
import Control.Monad.Fix
{-
plus :: Int -> Int -> Int 
plus n 0 = n
plus n m = plus (succ n) (pred m)
-}
-- show Fluffy instances
class Fluffy f where
  furry :: (a -> b) -> f a -> f b

plus' x y = fix (\f n m ->
  if m==0
    then n 
    else (f (succ n) (pred m))) x y


--q2: using lambda calculus



--q3: 

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = (\x -> f $ g x )


-- show Additional Misty functions
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

-- Exercise 6
-- Relative Difficulty: 3
-- (use banana and/or unicorn)
furry' :: (Misty m) => (a -> b) -> m a -> m b
furry' f = banana $ unicorn.f



-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f g h = ( f $ g h) h 
  unicorn f _  = f 

newtype State s a = State {
  state :: (s -> (s, a))
}


-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana f (State g) = State $ \x -> state ((f.snd.g) x) x
  unicorn a          = State $ \x -> (x,a)

