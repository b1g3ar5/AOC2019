{-# LANGUAGE BangPatterns, BlockArguments, PatternSynonyms, RecordWildCards, ScopedTypeVariables, ViewPatterns #-}

module Search where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Array (Array, Ix)
import Data.Array.ST (STArray, runSTArray, newArray, readArray, writeArray)
import qualified Data.Array as A
import Control.Monad.ST
import Control.Monad

class (Eq a, Ord a, Semigroup a) => Weight a where
  infty :: a

  updateWeight :: a -> a -> a
  updateWeight = const  

data Dijkstra i c = Dijkstra
    { bounds      :: (i, i)
    , startWeight :: i -> c
    , next        :: i -> c -> [(c, i)]
    }  

--type PQueue :: Type -> Type

emptyQ     :: PQueue a
singletonQ :: Ord a => a -> PQueue a
insertQ    :: Ord a => a -> PQueue a -> PQueue a

pattern EmptyQ :: PQueue a
pattern (:<)   :: Ord a => a -> PQueue a -> PQueue a    

newtype PQueue a = PQueue (Set a)

emptyQ = PQueue S.empty
singletonQ = PQueue . S.singleton
insertQ x (PQueue s) = PQueue (S.insert x s)

minView :: PQueue a -> Maybe (a, PQueue a)
minView (PQueue s) =
  case S.minView s of
    Just (x, s') -> Just (x, PQueue s')
    Nothing      -> Nothing

pattern EmptyQ   <- (minView -> Nothing)
pattern (:<) x q <- (minView -> Just (x, q))


dijkstraFrom :: (Ix i, Weight c) => Dijkstra i c -> i -> Array i c
dijkstraFrom d s = dijkstra d s Nothing

dijkstraFromTo :: (Ix i, Weight c) => Dijkstra i c -> i -> i -> c
dijkstraFromTo d s e = dijkstra d s (Just e) A.! e

dijkstra
  :: (Ix i, Weight c) 
  => Dijkstra i c -> i -> Maybe i 
  -> Array i c
dijkstra (Dijkstra{..} :: Dijkstra i c) start cutoff = runSTArray do
  costs <- newArray bounds infty
  let zero  = startWeight start
      queue = singletonQ (zero, start)
  writeArray costs start zero
  aux costs queue
  pure costs

  where

  aux :: STArray s i c -> PQueue (c, i) -> ST s ()
  aux costs EmptyQ = pure ()
  aux costs ((uWeight, u) :< queue) | Just u == cutoff = pure ()
  aux costs ((uWeight, u) :< queue) = do
    uWeight' <- readArray costs u
    when (uWeight == uWeight') do
      let edges = next u uWeight'
      !queue <- foldM (relaxNeighbour costs uWeight') queue edges
      aux costs queue

  relaxNeighbour
    :: STArray s i c -> c -> PQueue (c, i) -> (c, i) 
    -> ST s (PQueue (c, i))
  relaxNeighbour costs uWeight !queue (uvWeight, v) = do
    let !vWeight = uWeight <> uvWeight
    vWeight' <- readArray costs v
    case vWeight `compare` vWeight' of
      GT -> pure queue
      EQ -> do
        writeArray costs v $ updateWeight vWeight' vWeight
        pure queue
      LT -> do
        writeArray costs v vWeight
        pure $ insertQ (vWeight, v) queue