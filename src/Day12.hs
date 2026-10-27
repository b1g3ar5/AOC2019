module Day12(
  day12
  ) where

import Data.Maybe
import Data.List hiding ((\\), insert)
import Data.List.Split
import Data.Ord

import Debug.Trace

period :: [Moon1] -> Int
period ms0 = go 1 (step1 ms0)
  where
    go !n ms = if ms == ms0 then n else go (n+1) (step1 ms)

-- Asteroid shooting
day12 :: IO ()
day12 = do
  -- A moon is position, velocity
  -- where position is (x, y, z) etc.
  let moons = (\p -> (p, (0, 0, 0))) <$> actual
  -- easier to do one dimension a a time
  let moonsx =  (\((x,y,z),(p,q,r))-> (x,p)) <$> moons
  let moonsy =  (\((x,y,z),(p,q,r))-> (y,q)) <$> moons
  let moonsz =  (\((x,y,z),(p,q,r))-> (z,r)) <$> moons
  let n1 = 1000
  let xs = abs <$> fst <$> (iterate step1 moonsx)!!n1
  let ys = abs <$> fst <$> (iterate step1 moonsy)!!n1
  let zs = abs <$> fst <$> (iterate step1 moonsz)!!n1

  let pes = zipWith3 (\x y z -> x+y+z) xs ys zs

  let ps = abs <$> snd <$> (iterate step1 moonsx)!!n1
  let qs = abs <$> snd <$> (iterate step1 moonsy)!!n1
  let rs = abs <$> snd <$> (iterate step1 moonsz)!!n1

  let kes = zipWith3 (\x y z -> x+y+z) ps qs rs

  putStrLn $ "Day12: Part1: Energy: " ++ show (sum $ zipWith (*) pes kes)

  putStrLn $ "Day12: Part2: Time: " ++ show (lcms $ period <$> [moonsx, moonsy, moonsz])
  return ()


lcms :: Integral a => [a] -> a
lcms as = foldl lcm 1 as

-- Finds the length of the repeated string
findRep :: Eq a => [a] -> Int
findRep v@(x:xs) = go ixs
  where
    -- Find all the incidences of the first element
    ixs = tail $ elemIndices x (x:xs)
    -- Finds the first ix that works
    go [] = -99
    go (ix:ixs) = if check ix then ix else go ixs

    -- Checks if the ix is a repeat by seeing
    -- if all the chinks of that length are equal
    check ix = and $ (\c-> c == (cs!!0)) <$> cs
      where
        cs = init $ chunksOf ix v


getData12 :: IO String
getData12 = do
  txt <- readFile $ "./Data/Day12.txt"
  return $ txt


step1 :: [Moon1]->[Moon1]
step1 ms = zipWith (\m v -> (fst m + v, v)) ms vs
  where
    vs = (\m -> go m ms) <$> ms
    go m ms = foldl (\acc n -> case compare (fst m) (fst n) of
                                 GT -> acc-1
                                 EQ -> acc
                                 LT -> acc+1
                     ) (snd m) ms

type Moon1 = (Int, Int)


test1 = [(-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1)]
test2 = [(-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3)]

actual = [(-6, 2, -9), (12, -14, -4), (9, 5, -6), (-1, -4, 9)]

-- <x=-6, y=2, z=-9>
-- <x=12, y=-14, z=-4>
-- <x=9, y=5, z=-6>
-- <x=-1, y=-4, z=9>
