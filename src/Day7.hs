module Day7 (
  day7
  ) where

import Data.List

import Tape
import Instruction

day71 :: IO ()
day71 = do
  tin <- getData7
  let ps = permutations [0,1,2,3,4]
  let xs = tryPhase tin <$> ps
  putStrLn $ "Day7: Part1:" ++ show (maximum xs)


day7 :: IO ()
day7 = do
  day71
  tin <- getData7
  let ps = permutations [5,6,7,8,9]
  let as = replicate 5 tin
  let outs = (`goPhase` as) <$> ps
  putStrLn $ "Day7: Part2:" ++ show (maximum outs)


goPhase :: [Int] -> [Tape] -> Int
--goPhase ps ts1 = (input $ outs!!0)!!(iix $ outs!!0)
goPhase ps ts1 = head (input $ head outs)
  where
    -- put the phases in
    ts2 = zipWith (\p t -> setInput [p] t) ps ts1
    -- add the initial input (0) to tape0
    t0 = addInput [0] (head ts2)
    -- runall 100 times
    outs = iterate runAll (t0:tail ts2)!!100



runAll :: [Tape] -> [Tape]
runAll ts = [out02, out11, out21, out31, out41]
  where
    -- run tape 0
    out00 = run in00
    -- take the output from t0 and put it in t1
    in11 = addInput (output out00) in10
    -- delete the output which has been used
    out01 = delOutput out00
    -- repeat for all the other tapes
    out10 = run in11
    in21 = addInput (output out10) in20
    out11 = delOutput out10

    out20 = run in21
    in31 = addInput (output out20) in30
    out21 = delOutput out20

    out30 = run in31
    in41 = addInput (output out30) in40
    out31 = delOutput out30

    out40 = run in41
    out02 = addInput (output out40) out01
    out41 = delOutput out40

    -- set the initial tapes
    in00 = head ts
    in10 = ts!!1
    in20 = ts!!2
    in30 = ts!!3
    in40 = ts!!4

-- This is for the first part
-- It runs the tapes once only
tryPhase :: Tape -> [Int] -> Int
tryPhase tin ps = head $ output outE
  where
    ampA = setInput [head ps, 0] tin
    outA = run ampA
    ampB = setInput [ps!!1, head $ output outA] tin
    outB = run ampB
    ampC = setInput [ps!!2, head $ output outB] tin
    outC = run ampC
    ampD = setInput [ps!!3, head $ output outC] tin
    outD = run ampD
    ampE = setInput [ps!!4, head $ output outD] tin
    outE = run ampE


getData7 :: IO Tape
getData7 = do
  txt <- readFile "./Data/Day7.txt"
  --let txt = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
  --let txt = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
  --let txt = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
  --let txt = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
  --let txt = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
  return $ mkTape $ read <$> words (fmap (\c->if c == ',' then ' ' else c) txt)
