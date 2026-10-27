{-# language TupleSections #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}

module Day24(
  day24
  ) where


import Control.Comonad.Store

import Control.Concurrent

import Grid


parseGrid :: String -> Grid Bool
parseGrid = mkGrid . getCoords


parseGrid2 :: String -> Grid2 Bool
parseGrid2 = mkGrid2 . (fromCoord <$>) . getCoords


type Binary = [Bool]

binaryToDecimal :: Binary -> Int
binaryToDecimal bs = foldr (\(b, p) acc -> acc + (if b then p else 0) ) 0 $ zip bs $ iterate (*2) 1


bioDiversity :: Grid Bool -> Binary
bioDiversity g = concat $ (\y -> (\x -> (x,y) `peek` g) <$> [0..(gridSize-1)]) <$> [0..(gridSize-1)]


day24 :: IO ()
day24 = do
  let grid = parseGrid input
  let (g, n) = goUntil grid [] 100
  putStrLn $ "Day24: Part1: " ++ show (binaryToDecimal $ bioDiversity g)

  let grid2 = parseGrid2 input
  putStrLn $ "Day24: Part2: " ++ show (bugCount2 $ go2 grid2 200)

go :: Grid Bool -> Int -> Grid Bool
go g n = if n == 0
          then
            g
          else
            go (step bugRule g) (n-1)

go2 :: Grid2 Bool -> Int -> Grid2 Bool
go2 g n = if n == 0
          then
            g
          else
            go2 (step2 bugRule2 g) (n-1)


goUntil :: Grid Bool -> [Binary] -> Int -> (Grid Bool, Int)
goUntil g bs n = if bd `elem` bs || n == 0
               then
                 (g, n)
               else
                 goUntil (step bugRule g) (bd:bs) (n-1)
  where
    bd = bioDiversity g


test1 = "....#\n#..#.\n#..##\n..#..\n#...."

input = "###..\n#...#\n.#.##\n##.#.\n#.###"

