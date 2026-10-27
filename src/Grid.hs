{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveFunctor #-}
{-# language DeriveAnyClass #-}
{-# language StandaloneDeriving #-}
{-# language DerivingVia #-}

module Grid (
  Grid
  , Grid2
  , step
  , step2
  , bugRule
  --, basicRule
  , bugRule2
  , render
  , render2
  , mkGrid
  , mkGrid2
  , getCoords
  --, getCoords3
  , bugCount
  , bugCount2
  , render
  , gridSize
  , fromCoord
  , maxLevel
  ) where


import Prelude hiding (Bounded)
import Data.Functor.Compose (Compose(..))
import Data.List (intercalate)
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store, StoreT(..), store, experiment, pos)
import Control.Comonad (Comonad(..))


import Data.Maybe ( catMaybes )
import qualified Data.Vector as V


gridSize :: Int
gridSize = 5


newtype Bounded a = Bounded {unBound :: [a]} deriving (Eq, Show, Functor, Foldable)


instance Distributive Bounded where
  distribute = distributeRep


instance Representable Bounded where
  type Rep Bounded = Int
  index (Bounded v) i = v !! i
  tabulate desc = Bounded $ desc <$> [0..(gridSize-1)]


-- The recursive levels, with the starting grid at maxLevel
newtype Levels a = Levels {unLevels :: V.Vector a} deriving (Functor, Foldable)


instance Distributive Levels where
  distribute = distributeRep


instance Representable Levels where
  type Rep Levels = Int
  index (Levels v) i = v V.! i
  tabulate desc = Levels $ V.generate (2*maxLevel + 1) desc


--newtype Compose f g a = Compose { getCompose :: f (g a) }
type Grid a = Store (Compose Bounded Bounded) a

--newtype Compose2 f g h a = Compose2 {getCompose2 ::  f (Compose g h a)} deriving (Generic, Generic1, Semigroup, Monoid, Functor)

type Compose2 f g h a = Compose f (Compose g h) a

--instance (Representable f, Representable g) => Representable (Compose f g) where
  --tabulate = Compose . tabulate . fmap tabulate . curry


--instance (Representable f, Representable g, Representable h) => Representable (Compose2 f g h) where
  -- tabulate :: (Rep (Compose2 f g h) -> a) -> Compose2 f g h a
  --tabulate = undefined --Compose2 . tabulate . fmap tabulate . second curry . curry


type Grid2 a = Store (Compose Levels (Compose Bounded Bounded)) a

type Coord = (Int, Int)
type Coord2 = (Int, (Int, Int))

fromCoord :: Coord -> Coord2
fromCoord (x,y) = (maxLevel, (x,y))

mkGrid :: [Coord] -> Grid Bool
mkGrid xs = store lookup (0, 0)
  where
    lookup crd = crd `elem` xs
    

mkGrid2 :: [Coord2] -> Grid2 Bool
mkGrid2 xs = store lookup (maxLevel, (0, 0))
  where
    lookup crd = crd `elem` xs
    

type Rule = Grid Bool -> Bool
type Rule2 = Grid2 Bool -> Bool


bugRule :: Rule
bugRule g =
  (alive && numNeighboursAlive == 1) || (not alive && (numNeighboursAlive == 1 || numNeighboursAlive == 2 ))
  where
    alive = extract g
    neighbours = experiment neighbourCoords g
    numNeighboursAlive = length . filter id $ neighbours


bugRule2 :: Rule2
bugRule2 g
  | snd (pos g) == (2,2) = False -- the centre tile is the recursive grid, never a bug
  | otherwise = (alive && numNeighboursAlive == 1) || (not alive && (numNeighboursAlive == 1 || numNeighboursAlive == 2))
  where
    alive = extract g
    neighbours = experiment (filter inLevels . neighbourCoords3) g
    numNeighboursAlive = length . filter id $ neighbours


bugCount :: Int -> Grid Bool -> Int
bugCount _ (StoreT (Identity (Compose g)) _) = sum $ (\(Bounded v) -> sum $ (\x -> if x then 1 else 0) <$> v) <$>  vv 
  where
    (Bounded vv) = g


step :: Rule -> Grid Bool -> Grid Bool
step = extend


bugCount2 :: Grid2 Bool -> Int
bugCount2 (StoreT (Identity g) _) = length . filter id $ foldr (:) [] g


step2 :: Rule2 -> Grid2 Bool -> Grid2 Bool
step2 = extend


render :: Grid Bool -> String
render (StoreT (Identity (Compose g)) _) = foldMap p g
  where 
    p :: Bounded Bool -> String
    p = (++ "\n") . foldMap (bool "." "#")


render2 :: Grid2 Bool -> String
render2 (StoreT (Identity (Compose g)) _) = intercalate "\n" $ V.toList $ unLevels (rend <$> g)
  where
    rend :: Compose Bounded Bounded Bool -> String
    rend gg = render $ StoreT (Identity gg) (0, 0)


getCoords :: String -> [Coord]
getCoords s = cs
  where
    ls = lines s
    cs = catMaybes $ concat $ (\(y, l) ->
                                  (\(x, c) ->
                                        if c == '#' then Just (y, x) else Nothing
                                   ) <$> zip [0..] l
                              ) <$> zip [0..] ls


-- Neighbours when there's no wrapping
neighbourCoords :: (Int, Int) -> [(Int, Int)]
neighbourCoords (0,0) = [(0,1), (1,0)]
neighbourCoords (0,4) = [(0,3), (1,4)]
neighbourCoords (4,0) = [(3,0), (4,1)]
neighbourCoords (4,4) = [(3,4), (4,3)]
neighbourCoords (0,y) = [(0,y-1), (1,y), (0,y+1)]
neighbourCoords (4,y) = [(4,y-1), (3,y), (4,y+1)]
neighbourCoords (x,0) = [(x,1), (x-1,0), (x+1,0)]
neighbourCoords (x,4) = [(x,3), (x-1,4), (x+1,4)]
neighbourCoords (x,y)= [(x-1, y),(x+1, y),(x,y-1),(x,y+1)]


maxLevel :: Int
maxLevel = 105


inLevels :: (Int, (Int, Int)) -> Bool
inLevels (l, _) = l >= 0 && l <= 2*maxLevel


-- Neighbours on the infinite tape of grids
-- level l+1 is the grid inside the centre tile, l-1 the one surrounding it
neighbourCoords3 :: (Int, (Int, Int)) -> [(Int, (Int, Int))]
neighbourCoords3 (l, p@(x, y)) = concatMap expand [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
  where
    expand (nx, ny)
      | nx < 0 = [(l-1, (1,2))]
      | nx > 4 = [(l-1, (3,2))]
      | ny < 0 = [(l-1, (2,1))]
      | ny > 4 = [(l-1, (2,3))]
      | (nx, ny) == (2,2) = inner
      | otherwise = [(l, (nx,ny))]
    inner = case p of
      (1,2) -> [(l+1, (0,j)) | j <- [0..4]]
      (3,2) -> [(l+1, (4,j)) | j <- [0..4]]
      (2,1) -> [(l+1, (i,0)) | i <- [0..4]]
      _     -> [(l+1, (i,4)) | i <- [0..4]]
