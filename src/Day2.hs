module Day2(
  day2
  )where


import Data.Map ((!), insert)

import Instruction
import Tape

day2 :: IO ()
day2 = do
  xs <- getData2 2
  --let xs = [1,1,1,4,99,5,6,0,99]
  let t = mkTape xs
  let out = program 12 2 t
  putStrLn $ "Day2: Part1: Array:" ++ show out
  let out2 = go2 0 t
  putStrLn $ "Day2: Part2: (100*noun+verb) = " ++ show (100 * fst out2 + snd out2)


go2 :: Int -> Tape -> (Int, Int)
go2 n t
  | n == 9999 = (-1,-1)
  | program noun verb t == 19690720 = (noun, verb)
  | otherwise = go2 (n+1) t
  where
    (noun, verb) = quotRem n 100


program :: Int -> Int -> Tape -> Int
program noun verb (Tape f b i o inV) = get (go (0, Tape f b i o newV))
  where
    newV = insert 2 verb $ insert 1 noun inV
    go :: (Int, Tape) -> Tape
    go (ix, t@(Tape _ _ _ _ v)) = if v!(ix*4) == 99 then
                     t
                     else go (ix+1, runInstruction (mkInstrution ix v) t)


getData2 :: Int -> IO [Int]
getData2 n = do
  txt <- readFile $ "./Data/Day" ++ show n ++ ".txt"
  return $ read <$> words (fmap (\c->if c == ',' then ' ' else c) txt)
