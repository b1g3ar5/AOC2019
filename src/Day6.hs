module Day6(
  day6
  ) where

import Data.List.Split
import Data.Foldable as F
import Data.Tree
import Data.List (nub, sortOn)

import Utils

-- Orbit counting problem
day6 :: IO ()
day6 = do
  txt <- readFile $ "./Data/Day6.txt"

  --let txt = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
  --let txt = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
  let orbits = readOrbit <$> (lines txt)

  let allplanets = (fst<$>orbits) ++ (snd <$> orbits)
  let planets = nub $ (fst<$>orbits) ++ (snd <$> orbits)

  let f0 = [newTree $ head orbits]
  let allOrbits = go f0 $ tail orbits
  --putStrLn $ "Day6: orbits\n" ++ show orbits
  --putStrLn $ "Day6: AllOrbits\n" ++ drawForest allOrbits
  --putStrLn $ "Day6: Depth\n" ++ drawForest ((show <$>) <$> (toDepth allOrbits))
  putStrLn $ "Day6: Part1: Total orbit depth: " ++ show (sumTree $ toDepth allOrbits)
  let pathsToSAN = head $ head $ (pathsToNode "SAN" <$> allOrbits)
  let pathsToYOU = head $ head $ (pathsToNode "YOU" <$> allOrbits)
  let remSAN = removeCommonWords pathsToSAN pathsToYOU
  let remYOU = removeCommonWords pathsToYOU pathsToSAN
  putStrLn $ "Day6: Part2: Distance to Santa: " ++ show (length remYOU + length remSAN - 2)

  return ()

  where
    go :: Forest String -> [(String, String)] -> Forest String
    go f [] = f
    go f (o:os) = go (insert o f) os

readOrbit :: String -> (String, String)
readOrbit s = (bits!!0, bits!!1)
  where
    bits = splitOn ")" s

sumTree :: Forest Int -> Int
sumTree [] = 0
sumTree ((Node x ms):ps) = x + sumTree ms + sumTree ps

toDepth :: Forest String -> Forest Int
toDepth frst = go 0 frst
  where
    go :: Int -> Forest String -> Forest Int
    go n [] = []
    go n ((Node p ms):[]) = [Node n $ go (n+1) ms]
    go n ((Node p ms):ps) = (Node n $ go (n+1) ms) : (go n ps)


pathsToNode :: Eq a => a -> Tree a -> [[a]]
pathsToNode x (Node y ns) = [[x] | x == y] ++ map (y:) (pathsToNode x =<< ns)

removeCommonWords xs ys = filter (\x -> x `notElem` ys) xs

newTree :: (String, String) -> Tree String
newTree (planet, moon) = Node planet [Node moon []]

isIn :: String -> Forest String -> Bool
isIn s [] = False
isIn s ((Node l ms):ts) = if s == l then
                            True
                            else
                              (isIn s ms) || (isIn s ts)


-- What happens when the satelite is there but not the planet?
insert :: (String, String) -> Forest String -> Forest String
insert (planet, satelite) []= [newTree (planet, satelite)]
insert orb@(planet, satelite) frst =
  case (isIn planet frst, isIn satelite frst) of
    (True, True) -> insertPlanet ft (planet, st)
    (True, False) -> insertPlanet ft (planet, st)
    (False, True) -> insertSatelite frst orb
    (False, False) -> (newTree orb) : frst
  where
    (st, ft) = getSatelite satelite frst


-- When the planet is in the forest
insertPlanet :: Forest String -> (String, Tree String) -> Forest String
insertPlanet (p1@(Node lab moons):os) orb@(planet, satelite) =
  if planet == lab then -- add satelite to the moons
    (Node lab $ satelite:moons ):os
    else
      if isIn planet moons then
        (Node lab $ insertPlanet moons orb):os
        else -- add orbit to the others
          p1:(insertPlanet os orb)


insertSatelite:: Forest String -> (String, String) -> Forest String
insertSatelite frst@(p1@(Node lab moons):os) orb@(planet, satelite) =
  if satelite == lab then
    (Node planet [p1]):os
    else
      if isIn satelite moons then
        (Node lab $ insertSatelite moons orb):os
        else -- add orbit to the others
          p1:(insertSatelite os orb)


-- Gets the tree of a satelite at return the rest
getSatelite :: String -> Forest String -> (Tree String, Forest String)
getSatelite sat [] = (Node sat [], [])
getSatelite sat (t@(Node p ms):ts) =
  if sat == p then
    (t, ts)
    else
      if isIn sat ms then
        (tms, (Node p fms):ts)
        else
          (tts, t:fts)
  where
    (tms, fms) = getSatelite sat ms
    (tts, fts) = getSatelite sat ts
