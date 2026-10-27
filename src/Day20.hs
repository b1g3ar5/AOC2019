{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances    #-}

module Day20(
  day20
  ) where

import Prelude hiding (lookup)
import Data.List (transpose)
import Data.Bifunctor (Bifunctor(first))
import HeapIndexed (Heap, singleton, union, fromList, extractMin)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust) 
import System.TimeIt (timeIt)


data Inout = In | Out deriving (Eq, Show, Ord)
type NodeLabel = (String, Inout)
type Coord = (Int, Int)

data Node = Node {lab :: NodeLabel, pos :: Coord} deriving (Eq, Show)

type Edge = (NodeLabel, NodeLabel, Dist)
type Graph = [Edge]
type Grid = Set Coord
type Dist = Maybe Int

type RouteA= (Dist, [NodeLabel])
type StateA = (Graph , Heap RouteA NodeLabel, Set NodeLabel, NodeLabel -> Bool)

type Level = Int
type NodeLabelB = (NodeLabel, Level)
type RouteB = (Dist, [NodeLabelB])
type StateB = (Graph , Heap RouteB NodeLabelB, Set NodeLabelB, NodeLabelB -> Bool)


getData :: IO [String]
getData = do
  txt <- readFile "./Data/Day20.txt"
  return $ lines txt


rdrop :: Int -> [a] -> [a]
rdrop n = reverse . drop n . reverse


parseGraph :: [String] -> [Node]
parseGraph rows = concat [tp, bm, lt, rt, tphole, bmhole, lthole, rthole]
  where
    nrows = length rows
    ncols = length $ head rows
    cols = transpose rows

    grid = rdrop 2 . drop 2 <$> rdrop 2 (drop 2 rows)

    ((x1, y1), (x2, y2)) = getHole grid

    vhole = drop y1 $ take y2 grid
    hole = drop x1 . take x2 <$> vhole 
    thole = transpose hole
    holerows = length hole
    holecols = length $ head hole

    filt :: Char -> [Node] -> [Node]
    filt c ns = filter (\n -> fst (lab n) /= [c,c])  ns

    tp, bm, lt, rt :: [Node]
    tp = filt ' ' $ zipWith3 (\ix c1 c2 -> Node ([c1, c2], Out) (ix, 0)) [0..] (drop 2 $ rows!!0) (drop 2 $ rows!!1)
    bm = filt ' ' $ zipWith3 (\ix c1 c2 -> Node ([c1, c2], Out) (ix, nrows-5)) [0..] (drop 2 $ rows!!(nrows-2)) (drop 2 $ rows!!(nrows-1))
    lt = filt ' ' $ zipWith3 (\ix c1 c2 -> Node ([c1, c2], Out) (0, ix)) [0..] (drop 2 $ cols!!0) (drop 2 $ cols!!1)
    rt = filt ' ' $ zipWith3 (\ix c1 c2 -> Node ([c1, c2], Out) (ncols-5, ix)) [0..] (drop 2 $ cols!!(ncols-2)) (drop 2 $ cols!!(ncols-1))
    tphole = filt '.' $ filt ' ' $ filt '#' $ zipWith3 (\ix c1 c2 -> Node ([c1, c2], In) (ix, y1-1)) [x1..] (hole!!0) (hole!!1)
    bmhole = filt '.' $ filt ' ' $ filt '#' $ zipWith3 (\ix c1 c2 -> Node ([c1, c2], In) (ix, y1+holerows)) [x1..] (hole!!(holerows-2)) (hole!!(holerows-1))
    lthole = filt '.' $ filt ' ' $ filt '#' $ zipWith3 (\ix c1 c2 -> Node ([c1, c2], In) (x1-1, ix)) [y1..] (thole!!0) (thole!!1)
    rthole = filt '.' $ filt ' ' $ filt '#' $ zipWith3 (\ix c1 c2 -> Node ([c1, c2], In) (x1+holecols, ix)) [y1..] (thole!!(holecols-2)) (thole!!(holecols-1))


--parseGraph' :: [String] -> [Node]
getHole :: [String] -> (Coord, Coord)
getHole grid = ((hx1, hy1), (hx2, hy2))
  where
    --nrows = length grid
    --ncols = length $ head grid
    --cols = transpose grid

    gety1 n [] = n
    gety1 n (l:ls) = if ' ' `elem` l then n else gety1 (n+1) ls
    
    getx1 n [] = n
    getx1 n (c:cs) = if c `notElem` ".#" then n else getx1 (n+1) cs
    
    gety2 n [] = n
    gety2 n (l:ls) = if ' ' `notElem` l then n else gety2 (n+1) ls
    
    getx2 n [] = n
    getx2 n (c:cs) = if c `elem` "#." then n else getx2 (n+1) cs

    hy1 = gety1 0 grid
    hx1 = getx1 0 $ grid !! hy1
    hy2 = gety2 hy1 $ drop hy1 grid
    hx2 = getx2 hx1 $ drop hx1 $ grid !! hy1


shave :: [[a]] -> [[a]]
shave css = s $ s <$> css
  where
    s :: [a] -> [a]
    s xs = drop 2 $ take (n-2) xs
      where
        n = length xs

parseGrid :: [String] -> Grid
parseGrid ls = S.fromList $ fst <$> filter snd (concat ((\(y, cs) -> (\(x, c) -> ((x,y), c=='.')) <$> zip [0..(gridWidth-1)] cs) <$> zip [0..(gridLength-1)] css))
  where
    nr = length ls
    nc = length $ head ls
    gridLength = nr -4
    gridWidth = nc - 4
    css = shave ls


-- One BFS per node gives the distances to all the other nodes
parseEdges :: Grid -> [Node] -> [Edge]
parseEdges g nodes = filter (\(_, _, d) -> isJust d) $ concatMap edgesFrom nodes
  where
    edgesFrom :: Node -> [Edge]
    edgesFrom (Node l p) = [(l, m, if fst l == fst m then Just 1 else M.lookup q dists) | Node m q <- nodes, m /= l]
      where
        dists = bfs g p


-- Distances from the start to every reachable square
bfs :: Grid -> Coord -> Map Coord Int
bfs g start = go (M.singleton start 0) 0 $ S.singleton start
  where
    go :: Map Coord Int -> Int -> Set Coord -> Map Coord Int
    go dists d frontier
      | S.null frontier = dists
      | otherwise = go (foldr (`M.insert` (d+1)) dists next) (d+1) next
      where
        --next = S.fromList [n | c <- frontier, n <- neighbours4 c, n `S.member` g, n `M.notMember` dists]
        next = S.filter (`M.notMember` dists) $ S.filter (`S.member` g) $ S.unions $ S.map (S.fromList .neighbours4) frontier


day20 :: IO ()
day20 = do
  ls <- getData
  let nodes = parseGraph ls
      grid = parseGrid ls
      edges :: Graph
      edges = parseEdges grid nodes
  putStrLn $ "Day20: part1: " ++ show (fst $ dijkstraA (edges, singleton (Just 0, []) ("AA", Out), S.empty, (==("ZZ", Out))))
  putStrLn $ "Day20: part2: " ++ show (fst $ dijkstraB (edges, singleton (Just 0, []) (("AA", Out), 0), S.empty, (==(("ZZ", Out), 0))))

  return ()


-- Make Nothing represent infinity
-- <> is addition
-- mempty is infinity so that ord/min works
instance {-# overlapping #-} Semigroup Dist where
  Nothing <> Nothing = Nothing 
  Nothing <> _ = Nothing 
  _ <> Nothing = Nothing 
  Just x <> Just y = Just $ x+y


instance {-# overlapping #-} Monoid Dist where
  mempty = Nothing


instance {-# overlapping #-} Ord Dist where
  Nothing <= Nothing = False
  Nothing <= Just _ = False
  Just _ <= Nothing = True
  Just x <= Just y = x <= y


-- To find the route throught the graph
dijkstraA :: StateA -> RouteA
dijkstraA (graph, pipeline, visited, finished)
  | null pipeline = mempty
  | finished position = (savedMin, reverse $ position:route)
  | position `S.member` visited = dijkstraA (graph, remainingPipeline, visited, finished)
  | otherwise = dijkstraA (graph, newPipeline, S.insert position visited, finished)
  where
    (((savedMin, route), position), remainingPipeline) = fromJust $ extractMin pipeline
    ns :: [(Dist, NodeLabel)]
    ns = (\(_,e,d) -> (d,e)) <$> filter (\(s, e, _) -> s == position && e `S.notMember` visited) graph 
    newPipeline = remainingPipeline `union` fromList (first (\d -> (savedMin <> d, position:route)) <$> ns)


dijkstraB :: StateB -> RouteB
dijkstraB (graph, pipeline, visited, finished)
  | null pipeline = mempty
  | finished position = (savedMin, reverse $ position:route)
  | position `S.member` visited = dijkstraB (graph, remainingPipeline, visited, finished)
  | otherwise = dijkstraB (graph, newPipeline, S.insert position visited, finished)
  where
    --(((savedMin, route), (position, level)), remainingPipeline) = fromJust $ extractMin pipeline
    (((savedMin, route), position), remainingPipeline) = fromJust $ extractMin pipeline
    ns :: [(Dist, NodeLabelB)]
    ns = filter (\(_, e) -> e `S.notMember` visited) $ successors graph position
    newPipeline = remainingPipeline `union` fromList (first (\d -> (savedMin <> d, position:route)) <$> ns)


successors :: Graph -> NodeLabelB -> [(Dist, NodeLabelB)]
successors graph (label, level)
  | level == 0 = filter (\(_, ((e,eio),nextLevel)) -> nextLevel == 1 || e == "ZZ" || eio == In) ps
  | otherwise = filter (\(_, ((e,_),_)) -> e /= "AA" && e /= "ZZ") ps
  where
    ns = filter (\(s,_,_) -> s == label) graph
    ps = (\((s,_),(e,eio),d) -> if s/=e 
                                    then (d, ((e,eio),level)) 
                                    else (d, ((e,eio),if eio==In then level-1 else level+1)) 
         ) <$> ns



neighbours4 :: Coord -> [Coord]
neighbours4 (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]  