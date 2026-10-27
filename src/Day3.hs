module Day3(
  day3
) where

import Data.List
import Path

day3 :: IO ()
day3 = do
  (p1, p2) <- getData3
  let c1 = pcells (0, 0) p1
  let c2 = pcells (0, 0) p2
  let c1Sorted = sort c1
  let c2Sorted = sort c2
  let is = go [] c1Sorted c2Sorted
  let minIntersection = minimum $ fmap manhattan0 is

  let times1 = fmap (\i->duration i c1) is
  let times2 = fmap (\i->duration i c2) is
  let totals = zipWith (\t1 t2 -> t1+t2) times1 times2
  let minTotal = minimum totals

  putStrLn $ "Day3: Part1: minimum intersection distance: " ++ show minIntersection
  putStrLn $ "Day3: Part2: minimum intersection time: " ++ show minTotal

  where
    go :: [Position] -> [Position] -> [Position] -> [Position]
    go is [] cs = is
    go is cs [] = is
    go is c1@(c:cs) c2@(d:ds) =
      case compare c d of
        GT -> go is c1 ds
        EQ -> go (c:is) cs ds
        LT -> go is cs c2


getData3 :: IO (Path, Path)
getData3 = do
  txt <- readFile $ "./Data/Day3.txt"
  --let txt = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
  --let txt = "R8,U5,L5,D3\nU7,R6,D4,L4"
  --let txt = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  let ls = lines txt
  let p1 = pread $ ls!!0
  let p2 = pread $ ls!!1
  return (p1, p2)
