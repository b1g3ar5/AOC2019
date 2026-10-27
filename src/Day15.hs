{-# LANGUAGE DeriveFunctor #-}

module Day15(
  day15
  ) where


import Prelude hiding (filter, lookup)
import Data.List as List (map, foldl, (\\), intercalate, maximumBy, filter)
import Data.Maybe (isNothing)
import Data.Ord (comparing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M 

import Intcode

type Pos = (Int, Int)

isNeighbour :: Pos -> Pos -> Bool
isNeighbour (x1, y1) (x2, y2) = abs (x2-x1) + abs (y2-y1) == 1


type Space = Map Pos Item
type Item = Int -- Wall | Empty | Oxygen
type Visited = Map Pos Pos


getPoints :: ((Map Int Int, Int, Int), ([Int], [Int])) -> Space -> Pos -> Space
getPoints ((prog, y, z), (inputs, outputs)) lmap pos
  | y == -1 = lmap 
  | lastObj == 0 = lmap
  | otherwise = List.foldl M.union M.empty $ List.map (uncurry $ calcNext ((prog, y, z), (inputs, outputs)) lmap) (unmappedNeighbors lmap pos)
  where
    lastObj = if null outputs then -1 else head outputs
    unmappedNeighbors m (x, y) = filter (\(_, p) -> isNothing $ M.lookup p m) [(1, (x, y+1)), (2, (x, y-1)), (3, (x-1, y)), (4, (x+1, y))]


calcNext :: ((Map Int Int, Int, Int), ([Int], [Int])) -> Space -> Int -> Pos -> Space
calcNext (state, (inputs, outputs)) lmap dir newPos = getPoints (nextState, (newInput, newObj:newOutputs)) (M.insert newPos newObj lmap) newPos
    where (nextState, (newInput, newObj:newOutputs)) = compute state (dir:inputs, outputs)


bfs :: Space -> Visited -> Pos -> Item -> Visited
bfs lmap seen pos dis = if lastObj == 0 then seen else List.foldl M.union M.empty $ List.map (\p -> bfs lmap (M.insert p (lmap M.! p, dis+1) seen) p (dis+1)) (unmappedNeighbors seen pos)
  where
    lastObj = lmap M.! pos
    unmappedNeighbors m (x, y) = filter (\p -> isNothing $ M.lookup p m) [(x, y+1), (x, y-1), (x-1, y), (x+1, y)]


getMaxDis :: Space -> Pos -> Int -> Int
getMaxDis lmap pos item = snd $ maximumBy (comparing snd) $ M.elems $ M.filter ((== item) . fst) $ bfs lmap (M.singleton pos (2, 0)) pos 0


day15 :: IO ()
day15 = do
  parsed <- getTape

  let lmap = getPoints (startingState parsed []) (M.singleton (0, 0) 1) (0, 0)
  let os = fst <$> M.toList (M.filter (== 2) lmap)
      es = fst <$> M.toList (M.filter (== 1) lmap)
  --putStrLn $ showMap lmap
  --print os
  putStrLn $ "Day15: Part1 : " ++ show (getMaxDis lmap (0, 0) 2)
  putStrLn $ "Day15: Part2: " ++ show (fillWithO es os)


fillWithO :: [Pos] -> [Pos] -> Int
fillWithO = go 0
  where
  go :: Int -> [Pos] -> [Pos] -> Int
  go n es os
    | null es = n
    | otherwise = go (n+1) (es \\ newOs) $ os ++ newOs
    where
      newOs = filter (\e -> or $ isNeighbour e <$> os) es



showItem :: Int -> Char
showItem 0 = '#'
showItem 1 = '.'
showItem 2 = 'O'
showItem n = error $ "Can only show 0, 1 and 2, not: " ++ show n

showMap :: Map (Int, Int) Int -> String
showMap mp = intercalate "\n" $ (\y -> (\x -> dlookup (x,y) cmap) <$> [-21..19]) <$> [-19..21]
  where
    cmap = showItem <$> mp
    dlookup = M.findWithDefault '#'



getTape :: IO (Map Int Int)
getTape = do
  txt <- readFile $ "./Data/Day15.txt"
  return $ M.fromList $ zip [0..] $ read <$> words (fmap (\c->if c == ',' then ' ' else c) txt)


{-

41*41
#########################################
#.................#...#.....#...........#
#.###.###.#######.#.#.###.#.###.###.###.#
#...#...#.#.....#.#.#.....#...#.#...#O#.#
#.#####.###.###.#.#.#########.###.###.#.#
#.#...#.....#...#...#.#.....#...#...#.#.#
#.#.#.#.#####.#######.#.###.###.###.#.#.#
#.#.#.#...#.#.#.......#.#.#...#.#...#...#
###.#.###.#.#.#####.#.#.#.###.#.#.#####.#
#...#...#...#.#...#.#.#.#...#...#.#...#.#
#.#####.#####.#.#.#.###.#.#.#####.#.#.###
#.#...........#.#...#...#.#.#.....#.#...#
#.#############.#####.###.#.#.#.###.###.#
#...#...#...#.......#.#...#.#.#.#...#...#
#.#.#.#.#.#.#.#####.#.###.#.###.#.###.###
#.#...#...#.#.#...#...#...#.....#.#...#.#
#.#########.#.#.#.#####.#######.#.###.#.#
#.#...#.....#.#.#.......#.#.....#...#...#
#.###.#.#####.###.#.#####.#.#######.###.#
#.....#.....#.....#.#...#.....#.....#.#.#
#########.#.#####.#####.#######.#####.#.#
#...#...#.#.....#.#...#.......#.#.....#.#
#.#.#.#.#######.#.#.#.#######.#.#.#.###.#
#.#.#.#.........#.#.#.......#...#.#.....#
#.#.#.#.###########.#######.#####.#######
#.#...#.#.......#...#.....#.....#.#.....#
#.#######.#####.#.#####.#.#.#####.#####.#
#.#.....#.#...#...#.....#.#.....#.#.....#
#.#.###.#.#.#########.###.#####.#.#.#####
#.#.#...#.#...........#.#...#.#...#.....#
#.#.#.###.#.#######.###.###.#.#########.#
#...#.....#...#...#.#...#...#.........#.#
#.#############.#.#.#.#.#.###.#######.#.#
#.......#...#...#...#.#...#.......#.#...#
#######.#.#.#.#######.#########.#.#.###.#
#.....#...#.#.....#.#...#.....#.#.#.#...#
#.#########.#.###.#.###.###.#.###.#.#.###
#...#.......#.#...#...#.#...#.....#.#...#
#.#.#.#########.###.#.#.#.#########.###.#
#.#.............#...#.....#.............#
#########################################
-}


