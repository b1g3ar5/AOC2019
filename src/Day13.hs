{-# LANGUAGE BangPatterns #-}

module Day13(
  day13
  ) where

import Prelude hiding (filter)
import System.TimeIt (timeIt)
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as M
import Data.Map.Strict (Map, filter, size, toList, findWithDefault, fromList, union)
import Tape ( Tape(output), mkTape, setInput, isFinished, delOutput )
import Instruction (run)


-- Just move in the right direction
logic :: Pos -> Pos -> Int
logic b p
  | fst b > fst p = 1
  | fst b < fst p = -1
  | otherwise = 0


play :: Screen -> Tape -> Screen
play scr t
  | isFinished t = newScr
  | otherwise    = play newScr $ run $ setInput [logic b p] (delOutput t)
  where
    newScr = union (getScreen t) scr   -- new tiles win
    b = getBall newScr
    p = getPaddle newScr


day13 :: IO ()
day13 = do
  t1 <- getTape1
  t2 <- getTape2
  let o1 = output $ run t1
      s1 :: Screen
      s1 = fromList $ (\l -> ((head l, l!!1), decodeTile $ l!!2)) <$> chunksOf 3 o1

  putStrLn $ "Day13: Part1: " ++ show (size $ filter (==Block) s1)
  putStrLn $ "Day13: Part2: " ++ show (getScore $ play (M.empty) $ run $ setInput [0] t2)

  return ()


getBall :: Screen -> Pos
getBall scn = fst $ head (toList $ filter (==Ball) scn)


getPaddle :: Screen -> Pos
getPaddle = fst . head . toList . filter (==Paddle)


getScore :: Screen -> Int
getScore scr = case t of
                Score n -> n
                _ -> error $ "This is no score tile: " ++ show t
  where
    t = getTile scr (-1,0)


getScreen :: Tape -> Screen
getScreen t = fromList tileData
  where
    screenData = chunksOf 3 $ output t
    tileData = (\l -> ((head l, l!!1), decodeTile $ l!!2)) <$> screenData


data Tile = Empty | Wall | Block | Ball | Paddle | Score Int deriving (Eq, Show)


decodeTile :: Int -> Tile
decodeTile 0 = Empty
decodeTile 1 = Wall
decodeTile 2 = Block
decodeTile 3 = Paddle
decodeTile 4 = Ball
decodeTile n = Score n


type Pos = (Int, Int)


type Screen = Map Pos Tile


getTile :: Screen -> Pos -> Tile
getTile s p = findWithDefault Empty p s


getTape1 :: IO Tape
getTape1 = do
  txt <- readFile "./Data/Day13.txt"
  return $ mkTape $ read <$> words (fmap (\c->if c == ',' then ' ' else c) txt)


getTape2 :: IO Tape
getTape2 = do
  txt <- readFile "./Data/Day13.txt"
  return $ mkTape $ 2 : tail (read <$> words (fmap (\c->if c == ',' then ' ' else c) txt))
