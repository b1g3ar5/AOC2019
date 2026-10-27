module Day8(
  day8
  ) where

import Data.List.Split
import Data.Foldable as F
import Data.Tree
import Data.List (nub, sortOn)

import Data.List
import Utils

-- Orbit counting problem
day8 :: IO ()
day8 = do
  txt <- readFile $ "./Data/Day8.txt"
  --let txt = "0222112222120000"

  let row = 25
  let col = 6
  let layers = chunksOf (row*col) txt
  let pixels = transpose layers
  let pix = color <$> pixels
  let zeroCounts = init $ count '0' <$> layers
  let minZeroCount = minimum zeroCounts
  let ix = head $ elemIndices minZeroCount zeroCounts

  putStrLn $ "Day8: Part1: " ++ show ((count '1' $ layers!!ix)*(count '2' $ layers!!ix))
  putStrLn $ "Day8: Part2:\n" ++ (intercalate "\n" $ chunksOf row $ rep pix)

  return ()

rep :: String -> String
rep ss = (\s -> if s=='0' then ' ' else '#') <$> ss

color :: String -> Char
color (s:ss) = if s /= '2' then s else color ss

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (\y -> y==x) xs
