module Day9(
  day9
  ) where

import Tape
import Instruction

-- Orbit counting problem
day9 :: IO ()
day9 = do
  --ss <- getLine
  --let n = read ss::Integer
  txt <- readFile $ "./Data/Day9.txt"
  --let txt = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
  --let txt = "1102,34915192,34915192,7,4,7,99,0"
  --let txt = "104,1125899906842624,99"
  let tin0 = mkTape $ read <$> words ((\c->if c == ',' then ' ' else c) <$> txt)

  putStrLn $ "Day9: Part1: " ++ show (output $ run $ setInput [1] tin0)
  putStrLn $ "Day9: Part2: " ++ show (output $ run $ setInput [2] tin0)

  return ()
