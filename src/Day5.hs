module Day5 (
  day5
  ) where

import Data.List
import Control.Monad
import Data.Vector (Vector(..), fromList, (!), (//))

import Utils
import Tape
import Opcode
import Instruction

day5 :: IO ()
day5 = do
  tin <- getData5
  let tset1 = setInput [1] tin
  let tout1 = run tset1

  putStrLn $ "Day5: Part1: Output is:" ++ show (last $ output tout1)

  let tset2 = setInput [5] tin
  let tout2 = run tset2
  --putStrLn $ "tout ->" ++ show tout
  putStrLn $ "Day5: Part2: Output is:" ++ show (last $ output tout2)


getData5 :: IO Tape
getData5 = do
  txt <- readFile $ "./Data/Day5.txt"
  return $ mkTape $ read <$> words (fmap (\c->if c == ',' then ' ' else c) txt)
