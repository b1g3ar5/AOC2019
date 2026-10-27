module Day10(
  day10
  ) where

import Data.Maybe
import Data.List
import Data.Ord

-- Asteroid shooting
day10 :: IO ()
day10 = do
  asteriods0 <- getMap
  let (laser, asteroidCount) = getBest $ inSight asteriods0
  putStrLn $ "Day10: Part1: : " ++ show (laser, asteroidCount)
  let asteriods = filter (\v->v /= laser) asteriods0
  let bs = ((\a -> (a, baring $ a .- laser)) <$> asteriods)
  let sorted = sortBy (comparing snd) bs
  let grouped = groupBy (\x y -> (fst $ snd x)==(fst $ snd y)) $ sortBy (comparing snd) bs
  let shot = shoot grouped
  let ans@(Vec x y, (brg, dist)) = shot!!199
  putStrLn $ "Day10: Part2: shot[199] = " ++ show (x*100-y)
  return ()


shoot :: [[(Vec, (Double, Double))]] -> [(Vec, (Double, Double))]
shoot as = go [] as
  where
    go acc [] = acc
    go acc ([]:ys) = go acc ys
    go acc ((x:xs):ys) = go (acc++[x]) (ys ++ [xs])


getBest :: (Ord b, Eq a)=> [(a,b)]->(a,b)
getBest ts = ts!!ix
  where
    mn = maximumBy (\t1 t2 -> compare (snd t1) (snd t2)) ts
    ix = head $ elemIndices mn ts


data Vec = Vec Double Double deriving (Show, Eq)

(.-) :: Vec -> Vec -> Vec
(Vec x1 y1) .- (Vec x2 y2) = Vec (x1 -x2) (y1 -y2)


baring :: Vec -> (Double, Double)
baring (Vec x y) = (b, d)
  where
    b = case (x, y) of
          (0, 0) -> error "Asteriods are at the same place"
          (0, y) -> if y > 0 then 0 else pi
          (_, 0) -> if x > 0 then pi/2 else 3*pi/2
          (_, _) -> (pi/2 - atan (y/x))+(if x<0 then (if y>0 then pi else pi) else 0)
    d = (y*y+x*x)**0.5


inSight :: [Vec] -> [(Vec, Int)]
inSight asteriods = fmap (\v1 -> (v1, length $ nub $ fmap (\v2 -> fst $ baring $ v2 .- v1) $ exclude v1 asteriods)) asteriods


exclude :: (Eq a) => a -> [a] -> [a]
exclude y xs = [x | x<-xs, x /= y]


getMap :: IO [Vec]
getMap = do
  txt <- readFile $ "./Data/Day10.txt"
  --let txt = test3
  let ls = lines txt
  let rows = length ls
  let cols = length $ ls!!0
  let pos = concatMap (\y -> fmap (\x -> if (ls!!y)!!x == '#' then
                                              Just (Vec (fromIntegral x) (fromIntegral (-y)))
                                              else Nothing) [0..(rows-1)]
                         ) [0..(cols-1)]
  return $ catMaybes pos


test1 = ".#..#\n.....\n#####\n....#\n...##"
test2 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
test3 = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
test4 = ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#....#....###..\n..#.#.....#....##"
