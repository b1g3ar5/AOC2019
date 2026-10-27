{-# LANGUAGE FlexibleContexts #-}

module State(
  stateMain
  ) where

import Control.Monad ( forM )
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe

{-

So, what can we do with this in a Coalgebra?
We have:

makeRouteTree :: Coalgebra TreeF (Nodelabel, KeyMaps)

and:

type Coalgebra f a = a -> f a

Which turns oout to be:

(Nodelabel, KeyMaps) -> TreeF (Nodelabel, KeyMaps)

So we would need TreeF to be a State monad somehow.
But:

data TreeF a = NodeF Nodelabel [a] deriving (Functor, Show)



-}

newtype Graph a = Graph (Map.Map a [a]) deriving (Ord, Eq, Show)
data Tree a = Tree a [Tree a] deriving (Ord, Eq, Show)

-- evalState :: State s a	-> s -> a
-- s is the initial value of the state
dfs :: (Ord a) => Graph a -> a -> Tree a
dfs (Graph adj) start = evalState (dfs' start) (Set.singleton start)
  where
    neighbors x = Map.findWithDefault [] x adj
    --dfs' :: a -> State (Set.Set a) (Tree a)
    dfs' x = Tree x . catMaybes <$>
      forM (neighbors x) (\y -> get >>= \vis ->
                            if Set.member y vis
                            then return Nothing
                            else put (Set.insert y vis) >> Just <$> dfs' y
                         )


data MinDist = MinDist {i::Int} deriving (Show, Eq)

type MyState a = StateT MinDist IO a
type Selector a = (MyState a, a -> MyState ())


-- StateT s m a :: StateT (s -> m (a, s))
-- gets :: (s -> a) -> m a
-- modify :: (s -> s) -> m ()
-- s :: MinDist
-- m :: IO
-- a :: Int
aSelector :: Selector Int
aSelector = (gets i, \x -> modify (\vs -> vs {i=x}))
-- gets i = (s->a)->ma applied to  i :: MinDist -> Int
-- gives m Int = MyState Int

-- modify :: (s->s)
-- So, vs is a MinDist and we just set i in it to x.


getState :: Selector a -> MyState a
getState = fst

setWith :: Selector a -> (a -> a) -> MyState ()
setWith (getter, setter) f = do
  st <- getter
  setter (f st)

sample :: MyState ()
sample = do
  -- getState :: Selector a -> MyState a
  -- aSelector Selector Int
  -- getState aSelector :: MyState Int
  -- a = Int
  a <- getState aSelector
  -- setWith :: Selector a (a -> a) -> MyState ()
  -- aSelector :: MyState Int
  -- line below :: MyState ()
  setWith aSelector (\x -> x * a)
  b <- getState aSelector
  -- liftIO shortcut to IO monad from whatever monad oyu are in
  liftIO $ print b


stateMain :: IO ()
stateMain = do
  ret <- runStateT sample (MinDist 2)
  putStrLn $ show ret

{-

runState :: s -> (a,s)
sample :: MyState ()
MyState :: StateT MinDist IO a
MinDist :: MinDist Int

-}
  return $ fst ret


{-

newtype State s a = State { runState :: s -> (a,s) }


instance Functor (State s) where
  fmap  = smap


smap :: (a -> b) -> State s a -> State s b
smap f (State sf) = State go
  where
    --go :: s -> (b, s)
    go x = (f y, t)
      where
        (y, t) = sf x


instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  (State s2fs) <*> (State s2xs) = State go
    where
      go x = (f y, u)
        where
          (f, t) = s2fs x
          (y, u) = s2xs t


instance Monad (State s) where
  return a = State $ \s -> (a, s)

  State act >>= k = State $ \s ->
    let (a, s') = act s
    in runState (k a) s'


get :: State s s
get = State $ \s -> (s, s)


put :: s -> State s ()
put s = State $ \_ -> ((), s)


modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)


evalState :: State s a -> s -> a
evalState act = fst . runState act


execState :: State s a -> s -> s
execState act = snd . runState act

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

type Stack = [Int]

empty :: Stack
empty = []

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, [x])

push :: Int -> State Stack ()
push x = State $ \_ -> ((),[x])

tos :: State Stack Int
tos = State $ \(x:xs) -> (x, x:xs)

stackManip :: Int -> State Stack Int
stackManip x = do
    push x
    a <- pop
    if x > a
    then
      push a
    else
      push x
    tos

stateMain :: IO ()
stateMain = do
  (liftM push) 10
  let res = evalState (stackManip 10 >> stackManip 30) empty
  print res
-}
