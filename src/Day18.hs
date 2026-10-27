{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day18 where

import Data.Bifunctor
import Data.Bits (bit, (.&.), (.|.))
import Data.Char
import qualified Data.HashSet as HS
import Data.Hashable (Hashable, hashWithSalt)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.PQueue.Prio.Min as PQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple
import Data.Word (Word32)
import Debug.Trace
import qualified Queue as Q
import System.TimeIt

type Key = Char

type Keys = Set Key

type Coord = (Int, Int)

type Cell = Char

type Grid = Map Coord Cell

type KeySet = Word32

keyBit :: Key -> KeySet
keyBit '@' = bit 26
keyBit c = bit (ord c - ord 'a')

ksInsert :: Key -> KeySet -> KeySet
ksInsert c s = s .|. keyBit c

ksIsSubsetOf :: KeySet -> KeySet -> Bool
ksIsSubsetOf sub sup = sub .&. sup == sub

ksFromSet :: Set Key -> KeySet
ksFromSet = S.foldl' (\acc c -> acc .|. keyBit c) 0

neighbours :: Grid -> Coord -> [(Coord, Cell)]
neighbours g (x, y) = (\p -> (p, g M.! p)) <$> filter (`M.member` g) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

keyMap :: Grid -> Map Key Coord
keyMap g = M.fromList $ swap <$> M.assocs (M.filter (\c -> isKey c || isStart c) g)

isKey :: Cell -> Bool
isKey '#' = False
isKey '.' = False
isKey '@' = True
isKey c = isLower c

isStart :: Cell -> Bool
isStart '@' = True
isStart _ = False

isDoor :: Cell -> Bool
isDoor '#' = False
isDoor '.' = False
isDoor c = isUpper c

data Edge = Edge {destination :: Char, distance :: Int, keys :: Set Cell} deriving (Eq, Show)

data State = State {sPos :: Coord, sKeysRequired :: Keys} deriving (Eq, Ord, Show)

edgesFrom :: Grid -> Coord -> [Edge] -- [(Coord, Key, Int, Keys)]
edgesFrom grid start = go (Q.singleton (initial, 0)) (S.singleton $ sPos initial)
  where
    initial = State start S.empty
    go :: Q.Queue (State, Int) -> Set Coord -> [Edge]
    go Q.Empty _ = []
    go queue visited
      | (n /= 0) && isKey cell = (Edge cell n oldKeys) : go rest visited
      | otherwise = go newQueue newVisited
      where
        Just (((State p oldKeys), n), rest) = Q.pop queue
        cell = grid M.! p
        nextStates = step <$> neighbours grid p
        unseen = filter ((`S.notMember` visited) . sPos) nextStates
        newVisited = visited `S.union` (S.fromList $ sPos <$> unseen)
        newQueue = rest `Q.appendList` ((,n + 1) <$> unseen)
        step :: (Coord, Cell) -> State
        step (newPos, newCell)
          | isDoor newCell = State newPos (toLower newCell `S.insert` oldKeys)
          | otherwise = State newPos oldKeys

type Keys4 = (Key, Key, Key, Key)

keys4FromList :: [Key] -> Keys4
keys4FromList [a] = (a, '\0', '\0', '\0')
keys4FromList [a, b, c, d] = (a, b, c, d)
keys4FromList xs = error $ "keys4FromList: expected 1 or 4 keys, got " ++ show (length xs)

keys4ToList :: Keys4 -> [Key]
keys4ToList (a, b, c, d) = [a, b, c, d]

setKey4 :: Int -> Key -> Keys4 -> Keys4
setKey4 0 k (_, b, c, d) = (k, b, c, d)
setKey4 1 k (a, _, c, d) = (a, k, c, d)
setKey4 2 k (a, b, _, d) = (a, b, k, d)
setKey4 3 k (a, b, c, _) = (a, b, c, k)
setKey4 i _ _ = error $ "setKey4: index out of range: " ++ show i

data DState = DState {dKeys4 :: !Keys4, dCollected :: !KeySet} deriving (Ord, Eq, Show)

instance Hashable DState where
  hashWithSalt s (DState ks c) = s `hashWithSalt` ks `hashWithSalt` c

dijkstra :: [Grid] -> Int
dijkstra grids = go (PQ.singleton 0 (DState (keys4FromList ('@' <$ grids)) 0)) HS.empty
  where
    !allKeysMask = ksFromSet $ S.fromList [c | g <- grids, c <- M.elems g, isLower c]
    kmaps = keyMap <$> grids
    !edgeMaps4 = take 4 $ zipWith (\g km -> M.map ((precompEdge <$>) . edgesFrom g) km) grids kmaps ++ repeat M.empty
    precompEdge (Edge d dist req) = (d, dist, ksFromSet req)
    go :: PQ.MinPQueue Int DState -> HS.HashSet DState -> Int
    go pipeline visited
      | PQ.null pipeline = error "Ran out of possibles"
      | state `HS.member` visited = go rest visited
      | allKeysMask `ksIsSubsetOf` collected = n
      | otherwise = go newPipeline (state `HS.insert` visited)
      where
        ((n, state@(DState keys4 collected)), rest) = PQ.deleteFindMin pipeline
        newPipeline =
          foldl'
            (\q (d, s) -> PQ.insert d s q)
            rest
            [ (n + distance, DState newKeys newCollected)
            | (i, (curKey, em)) <- zip [0 ..] (zip (keys4ToList keys4) edgeMaps4),
              (newKey, distance, reqMask) <- M.findWithDefault [] curKey em,
              reqMask `ksIsSubsetOf` collected,
              let newCollected = ksInsert newKey collected,
              let newKeys = setKey4 i newKey keys4,
              not $ DState newKeys newCollected `HS.member` visited
            ]

day18 :: IO ()
day18 = do
  ls <- getDataA
  bss <- getDataB

  let g = parse ls
      gs = parse <$> bss

  putStrLn $ "Day18: Part1: " ++ show (dijkstra [g])
  putStrLn $ "Day18: Part2: " ++ show (dijkstra gs)

  return ()

getDataA :: IO [String]
getDataA = do
  txt <- readFile "./Data/Day18.txt"
  return $ lines txt

getDataB :: IO [[String]]
getDataB = do
  txt <- readFile "./Data/Day18b.txt"
  let ls = lines txt
      n = 40
  return [take (n + 1) $ take (n + 1) <$> ls, drop n $ take (n + 1) <$> ls, take (n + 1) $ drop n <$> ls, drop n $ drop n <$> ls]

parseCell :: Char -> Maybe Cell
parseCell c
  | c == '#' = Nothing
  | otherwise = Just c

catM :: [(a, Maybe b)] -> [(a, b)]
catM z = second fromJust <$> filter (isJust . snd) z

parse :: [String] -> Map Coord Cell
parse css = M.fromList $ catM (concatMap (\(row, cs) -> (\(col, c) -> ((col, row), parseCell c)) <$> zip [0 ..] cs) (zip [0 ..] css))
