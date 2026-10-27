{-# LANGUAGE DeriveFunctor #-}

module Day23(
  day23
  ) where


import Data.Set (Set)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Tape (Tape)
import Tape qualified as T
import Instruction (run)
import Debug.Trace


getTape :: IO Tape
getTape = do
  txt <- readFile $ "./Data/Day23.txt"
  return $ T.mkTape $ read <$> words ((\c->if c == ',' then ' ' else c) <$> txt)


type Address = Int
type Tapes = Map Address Tape
type NAT = (Int, Int)


runNext1 :: Address -> Tapes -> Int
runNext1 addr ts
  | null out = runNext1 nextAddr $ M.insert addr newt ts
  | target == 255 = payload !! 1
  | otherwise = runNext1 nextAddr $ M.insert addr (T.delOutput newt) newTapes
  where
    nextAddr = (addr + 1) `mod` 50
    t = ts M.! addr
    -- If there's no input, add -1
    newt = run $ if T.noInput t then T.addInput [-1] t else t
    out = chunks $ T.output newt
    target = fst $ head out
    payload = snd $ head out
    -- Add the output to the target tape
    newTapes = foldl (\tts (a,p) -> M.adjust (T.addInput p) a tts) ts out


runNext2 :: Address -> ((NAT, Int), Tapes) -> Int
runNext2 addr ((currentNAT, sentY), ts)
  | areIdle ts && currentY == sentY = sentY
  | areIdle ts = runNext2 0 ((currentNAT, currentY), M.adjust (T.addInput [currentX, currentY]) 0 ts)
  | otherwise = runNext2 nextAddr ((nextNAT, sentY), newTapes)
  where
    (currentX, currentY) = currentNAT
    nextAddr = (addr + 1) `mod` 50
    t = ts M.! addr
    -- If there's no input, add -1
    newt = run $ if T.noInput t then T.addInput [-1] t else t
    out = chunks $ T.output newt
    -- Only update the NAT if a packet was sent to 255
    nextNAT = case [(px, py) | (255, [px, py]) <- out] of
                [] -> currentNAT
                ps -> last ps
    -- Save this tape, then add the output to the target tapes
    newTapes = foldl (\tts (a,p) -> M.adjust (T.addInput p) a tts) (M.insert addr (T.delOutput newt) ts) out


areIdle :: Tapes -> Bool
areIdle ts = all T.noInput $ M.elems ts


chunks :: [Int] -> [(Int, [Int])]
chunks out
  | null out = []
  | otherwise = (target, payload) : chunks rest
  where
    target = head out
    payload = take 2 $ tail out
    rest = drop 3 out


day23 :: IO ()
day23 = do
  t <- getTape
  let ts :: Tapes
      ts = M.fromList [(i, [i] `T.addInput` t) | i <- [0..49]]
  putStrLn $ "Day23: Part1: " ++ show (runNext1 0 ts)
  putStrLn $ "Day23: Part2: " ++ show (runNext2 0 (((-1, -1), minBound), ts))


