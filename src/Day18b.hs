module Day18b where

import qualified Data.Map.Strict as Map 
import qualified Data.Set as Set 
import qualified Data.Sequence as Seq 
import Data.Sequence ((|>), ViewL(..)) 
import Data.Char (isLower, isUpper, toLower) 

type Pos = (Int, Int) 
type Keys = Set.Set Char 

data BFSNode = BFSNode { pos :: Pos , distance :: Int , required :: Keys } deriving (Eq, Show) 
data Edge = Edge { destination :: Char , edgeDistance :: Int , edgeRequired :: Keys } deriving (Eq, Show) 

type Grid = Map.Map Pos Char

isKey :: Char -> Bool
isKey c = c `elem` ['a'..'z']


bfsFrom :: Grid -> Pos -> [Edge] 
bfsFrom grid start = go (Seq.singleton initial) Set.empty 
  where 
    initial = BFSNode start 0 Set.empty 
    go queue visited = 
      case Seq.viewl queue of 
        EmptyL -> [] 
        node :< rest -> 
          let state = (pos node, required node) 
          in if Set.member state visited then go rest visited else 
            let visited' = Set.insert state visited 
                nextNodes = [ step node p | p <- neighbours grid (pos node) ] 
                found = [ Edge k d r | BFSNode p d r <- nextNodes , k <- [cellAt grid p] , isKey k, p /= start ] 
                queue' = foldl (|>) rest nextNodes 
          in found ++ go queue' visited' 
    step node p = 
      let r = required node 
          k = cellAt grid p
      in case isUpper k of 
          True -> BFSNode p (distance node + 1) (Set.insert (toLower k) r) 
          False -> BFSNode p (distance node + 1) r 
          
neighbours :: Grid -> Pos -> [Pos] 
neighbours grid (x, y) = filter passable [ (x + 1, y) , (x - 1, y) , (x, y + 1) , (x, y - 1) ] 
  where passable p = case Map.lookup p grid of 
                      Nothing -> False 
                      Just '#' -> False 
                      Just _ -> True 
                      
cellAt :: Grid -> Pos -> Char 
cellAt grid p = Map.findWithDefault '#' p grid