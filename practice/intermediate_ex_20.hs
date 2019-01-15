-- file: 
-- abuhay
import System.IO
class Fluffy f where
  -- fmap in Functor
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
-- (a->b) -> [a] -> [b]
-- furry _ [] = []
-- furry f xs = foldr ((:) . f) [] xs
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
-- furry :: (a -> b) -> Maybe a -> Maybe b
  furry _ Nothing  = Nothing
  furry f (Just x) = Just $ f x

-- Exercise 3
-- Relative Difficulty: 5
-- ((->) t) == (t ->)
instance Fluffy ((->) t) where
-- furry :: (a -> b) -> m a -> m b
-- furry :: (a -> b) -> (t -> a) -> (t -> b)
-- furry a2b ta = ta . a2b
  furry  = (.)
--furry f g = (f.g)
--furry f g = (\x -> f(g x))

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
-- (a->b) -> EitherLeft (Either a t) -> EitherLeft (Either b t)
  furry f (EitherLeft (Left a) ) = EitherLeft $ Left $ f a
  furry _ (EitherLeft (Right t)) = EitherLeft $ Right t

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
-- (a->b) -> EitherRight (Either t a) -> EitherRight (Either t b)
  furry f (EitherRight (Right x)) = EitherRight $ Right $ f x
  furry _ (EitherRight (Left x)) = EitherRight $ Left x

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a


-- Exercise 6
-- Relative Difficulty: 3
-- (use banana and/or unicorn)
--banana:: (a-> m b) -> m a -> m b
--unicorn::  b -> m b
-- f: (a -> b)
-- unicorn .  f :: (a -> b) -> m b 
furry' :: (Misty m) => (a -> b) -> m a -> m b
furry' f ma = banana (unicorn.f) ma 
--furry' f ma = banana (\a -> unicorn (f a)) ma 

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
--banana:: (a-> [b]) -> [a] -> [b]
--unicon::  a -> [a]
-- banana  = concatMap
-- unicorn = return
  banana f ma = concat $ furry f ma
  unicorn = (: []) 

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
-- (a-> Maybe b) -> Maybe a -> Maybe b
--  a -> Maybe a
  banana f (Just a) = f a
  banana _  Nothing = Nothing
--unicorn=return
  unicorn a  = Just a
 

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
--banana :: (a -> m b) -> m a -> m b
--banana :: (a -> t-> b) -> (t-> a) -> (t-> b)
--banana at2b t2a = \t -> let a = t2a t 
--                        in at2b a t
  banana at2b t2a t = at2b (t2a t) t
-- unicorn :: a -> m a
-- unicorn :: a -> (t -> a)
  unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
-- (a -> EitherLeft (Either b t)) -> EitherLeft (Either a t) -> EitherLeft (Either b t)
  banana f (EitherLeft (Left a)) =  f a 
  banana _ (EitherLeft (Right t)) = EitherLeft $ Right t
-- a -> EitherLeft (Either a t)
  unicorn =(\a -> EitherLeft $ Left a) 

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
-- (a -> EitherRight (Either t b)) -> EitherRight (Either t a) -> EitherLeft (Either t b)
  banana f (EitherRight (Right a)) = f a 
  banana _ (EitherRight (Left t)) = EitherRight $ Left t 
-- a -> EitherLeft (Either t a)
  unicorn = (\a -> EitherRight $ Right a) 

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean mm= banana id mm 

-- Exercise 13
-- Relative Difficulty: 6
--apple= banana.flip furry'
--banana: (a-> [b]) -> [a] -> [b]
--furry': (a -> b) -> m a -> m b
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mf = banana (\f -> furry' f ma) mf
--apple = banana . flip furry'

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] _     = unicorn []
moppy (x:xs) f = apple (moppy xs f) (furry' (:) (f x))
-- moppy (x:xs) f = banana (\ b -> banana (\bs -> unicorn (b:bs)) (moppy xs f)) (f x)
-- moppy (x:xs) f = banana2 (:) (f x) $ moppy xs f

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage ma  = moppy ma id
--sausage = flip moppy id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
-- apple: m b -> m (b -> c) -> m c
-- furry': (a -> b) -> m a -> m b
--       : (a -> (b -> c)) -> ma -> m (b -> c)
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb $ furry' f ma
-- banana2 = (flip apple . ) . furry'

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
-- apple: m c -> m (c -> d) -> m d
-- banana2: (a -> b -> (c -> d) -> m a -> m b -> m (c -> d)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc $ banana2 f ma mb 
-- banana3 = ((flip apple . ) .) . furry'

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
-- apple: m d -> m (d -> e) -> m e
-- banana3: (a -> b -> c -> (d -> e)) -> m a -> m b -> m c -> m (d -> e)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = apple md $ banana3 f ma mb mc
-- banana3 = (((flip apple . ) . ) . ) . furry'

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
-- furry: (a -> b) -> f a -> f b
-- furry: (a -> b) -> State s a -> State s b
-- furry: (a -> b) -> State {s -> (s, a)} -> State {s -> (s, b)}
  furry a2b sa = State $ \ s -> let (s', a) = state sa s
                                    b = a2b a
                                in (s', b)  

-- Exercise 20
-- Relative Difficulty: 10
-- banana :: (a -> m b) -> m a -> m b
instance Misty (State s) where
-- banana :: (a -> (State s) b) -> (State s) a -> (State s) b
-- banana :: (a -> State {s -> (s, b)}) -> State {s -> (s, a)} -> State {s-> (s,b)}
-- banana f st = jellybean $ furry' f st
  banana a2sb sa = State $ \ s ->  let (s', a) = state sa s
                                       sb = a2sb a
                                   in state sb s'

--unicorn :: a -> (State s) b
--unicorn :: a -> State {s -> (s,a)}
  unicorn a = State $ \ s -> (s, a)


--main :: IO ()
--main = do
