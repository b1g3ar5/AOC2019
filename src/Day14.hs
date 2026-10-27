{-# language DeriveFunctor #-}
{-# language TupleSections #-}

-- {-# LANGUAGE FlexibleInstances #-}

module Day14(
  day14
  ) where


import Data.List hiding (insert)
import Data.List.Split
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
--import Data.Tree

import Debug.Trace


-- Recursion library
--newtype Fix f = Fix { unFix :: f (Fix f) }
--type Coalgebra f a = a -> f a
--ana :: Functor f => Coalgebra f a -> a -> Fix f
--ana coalg = Fix . fmap (ana coalg) . coalg
--type Algebra f a = f a -> a
--cata :: Functor f => Algebra f a -> Fix f -> a
--cata alg = alg . fmap (cata alg) . unFix
--hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
--hylo f g = f . fmap (hylo f g) . g



day14 :: IO ()
day14 = do
  eqs <- getEqs
  let ore = 1000000000000::Int
  putStrLn $ "Day14: Part1: Ore: " ++ show (goFuel eqs 1)
  putStrLn $ "Day14: Part2: Fuel: " ++ show (solveForFuel eqs ore)



  return ()


solveForFuel :: [Eqn] -> Int -> Int
solveForFuel eqn target = go 1184168
  where
    go try = if (f < target) && (up > target) then
               try
               else
                 go $ try + (target - f) `div` df
      where
        f = goFuel eqn try
        up = goFuel eqn $ try + 1
        df = up - f


-- Works out the fuel from the ore store
goFuel :: [Eqn] -> Int -> Int
goFuel eqs fuel = sortout tms1
  where
    lvls = updateOrder eqs
    fuel0 = Tm "FUEL" fuel
    ss1 = subs eqs [fuel0]
    tms1 = substituteOre eqs  <$> ss1
    sortout = tcoeff . head . consolidate .concat



data Tm = Tm {tid::String, tcoeff::Int} deriving (Show, Ord)

instance Eq (Tm) where
  (Tm c i) == (Tm d j) = c==d

(.*.):: Int -> Tm -> Tm
n .*. (Tm s m) = Tm s (n*m)

(.+.):: Tm -> Tm -> Tm
(Tm t n) .+. (Tm s m) = if s == t then
                        Tm s (n+m)
                        else
                          error "Trying to add disimilat Tms"


data Eqn = Eqn { eid::String, coeff::Int, ts::[Tm]} deriving (Show, Eq)

-- Get all the (parent, child) pairs
toPairs :: Eqn -> [(String, String)]
toPairs (Eqn i _ ts) = (\(Tm j _) -> (i, j)) <$> ts


(.<>.) :: [Tm] -> [Tm] -> [Tm]
t0 .<>. t1 = (\g -> Tm (tid $ g!!0) (sum $ tcoeff <$> g)) <$> gpd
    where
      ts = t0 ++ t1
      gpd =  (groupBy (\a b -> (tid a)==(tid b)) ts)


updateOrder :: [Eqn] -> [String]
updateOrder eqns = concat $ reverse $ levels eqns


levels :: [Eqn] -> [[String]]
levels eqns = go [["FUEL"]]
  where
    go :: [[String]] -> [[String]]
    go lvls@(l:ls) = if next == ["ORE"]
                     then
                       lvls
                     else
                       go $ next:lvlNotInNext
      where
        next = nub $ concat $ dependents eqns <$> l
        lvlNotInNext = (\ss -> filter (\s -> s `notElem` next) ss) <$> lvls


dependents :: [Eqn] -> String -> [String]
dependents eqns x = if x == "ORE" then [] else tid <$> ts
  where
    (Eqn s c ts) = findEq x eqns

-- Start with fuel and then substitute in update order
subs :: [Eqn] -> [Tm] -> [Tm]
subs eqns ts = go ss ts
  where
    ss = updateOrder eqns
    go [] tms = tms
    go (s:ss) tms = go ss newTms
      where
        newTms = update1 s tms
    update1 :: String -> [Tm] -> [Tm]
    update1 s tms = consolidate $ concat $ newTms
      where
        newTms = (\tm -> if tid tm == s then
                           substituteNotOre eqns tm
                           else
                             [tm]
                 ) <$> tms


substituteOre :: [Eqn] -> Tm -> [Tm]
substituteOre eqs t@(Tm s c) = if s == "ORE" then
                                 [t]
                                 else
                                   (multiplier .*.) <$> (ts $ eqs!!ix)
  where
  ids = eid <$> eqs
  cs = coeff <$> eqs
  ix = case elemIndex s ids of
         Nothing -> error $ "s: " ++ (show s) ++ " is not in: " ++ (show ids)
         Just x -> x
  ecoeff = cs!!ix
  multiplier = ceiling $ (fromIntegral c)/(fromIntegral ecoeff)


substituteNotOre :: [Eqn] -> Tm -> [Tm]
substituteNotOre eqs t@(Tm s c) = if s == "ORE" || dependsOnOre then
                                    [t]
                                    else
                                      (multiplier .*.) <$> (ts $ eqs!!ix)
  where
  ids = eid <$> eqs
  cs = coeff <$> eqs
  ix = case elemIndex s ids of
          Nothing -> error $ "Can't find " ++ (show s) ++ " in " ++ (show ids)
          Just x -> x
  ecoeff = cs!!ix
  multiplier = ceiling $ (fromIntegral c)/(fromIntegral ecoeff)
  --extra = (fromIntegral multiplier) - (fromIntegral c)/(fromIntegral ecoeff)
  dependsOnOre = elem "ORE" $  tid <$> (ts $ eqs!!ix)


consolidate :: [Tm] -> [Tm]
consolidate lts = go [] lts
  where
    go :: [Tm] -> [Tm] -> [Tm]
    go acc [] = acc
    go acc (x@(Tm xid _):xs) = case ix  of
                                  Nothing -> go (acc ++ [x]) xs
                                  Just _ -> go ((\y@(Tm yid _) ->
                                                if xid==yid then
                                                  x .+. y
                                                  else
                                                    y
                                             ) <$> acc) xs

      where
        ids = tid <$> acc
        ix = elemIndex xid ids


getEqs :: IO [Eqn]
getEqs = do
  txt <- readFile $ "./Data/Day14.txt"
  let ls = lines txt

  let lines1 = ["10 ORE => 10 A"
              , "1 ORE => 1 B"
              , "7 A, 1 B => 1 C"
              , "7 A, 1 C => 1 D"
              , "7 A, 1 D => 1 E"
              , "7 A, 1 E => 1 FUEL"
              ]

  let lines2 = ["157 ORE => 5 NZVS"
               , "165 ORE => 6 DCFZ"
               , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
               , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
               , "179 ORE => 7 PSHF"
               , "177 ORE => 5 HKGWZ"
               , "7 DCFZ, 7 PSHF => 2 XJWVT"
               , "165 ORE => 2 GPVTF"
               , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]

  let lines3 = ["2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
             , "17 NVRVD, 3 JNWZP => 8 VPVL"
             , "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
             , "22 VJHF, 37 MNCFX => 5 FWMGM"
             , "139 ORE => 4 NVRVD"
             , "144 ORE => 7 JNWZP"
             , "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
             ,  "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
             , "145 ORE => 6 MNCFX"
             , "1 NVRVD => 8 CXFTF"
             , "1 VJHF, 6 MNCFX => 4 RFSQX"
             , "176 ORE => 6 VJHF"]

  let lines4 = ["171 ORE => 8 CNZTR"
             , "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
             , "114 ORE => 4 BHXH"
             , "14 VRPVC => 6 BMBT"
             , "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
             , "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
             , "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
             , "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
             , "5 BMBT => 4 WPTQ"
             , "189 ORE => 9 KTJDG"
             , "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
             , "12 VRPVC, 27 CNZTR => 2 XDBXC"
             , "15 KTJDG, 12 BHXH => 5 XCVML"
             , "3 BHXH, 2 VRPVC => 7 MZWV"
             , "121 ORE => 7 VRPVC"
             , "7 XCVML => 6 RJRHP"
             , "5 BHXH, 4 VRPVC => 5 LTCX"]

  return $ parseEqn <$> ls

parseTm :: String -> Tm
parseTm s = if (length w) > 1 then
                Tm (w!!1) (read $ w!!0)
                else
                  error $ "w too short in parseTerm: " ++ (show w)
  where
    w = words s


parseEqn :: String -> Eqn
parseEqn s = Eqn (tid r) (tcoeff r) $ ore2end ls
  where
    ps = splitOn "=>" s
    ls = parseTm <$> (splitOn "," $ ps!!0)
    r = if (length ps) > 1 then
          parseTm $ ps!!1
          else
            error $ "ps too short in parseEquation: " ++ (show ps)

ore2end :: [Tm] -> [Tm]
ore2end us = go [] us
  where
  go acc [] = acc
  go acc (t@(Tm s _):ts') = if s == "ORE" then
                             acc ++ ts' ++ [t]
                             else
                              go (acc ++ [t]) ts'

findEq :: String -> [Eqn] -> Eqn
findEq s [] = error $ "No such equation: " ++ s
findEq s (eq:eqs) = if (eid eq) == s then
                      eq
                      else
                        findEq s eqs
