{-# LANGUAGE DeriveFunctor #-}

module Lib
    ( libMain
    ) where

--import Data.Array
--import Data.List
--import Utils
--import Tree
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18 ( day18 )
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
--import State
--import Glover

libMain :: IO ()
libMain = do
  day1
  day2
  day3
  day4
  day5
  day6
  day7
  day8
  day9
  day10
  day11
  day12
  day13
  day14
  day15
  day16
  day17
  day18
  day19
  day20
  day21
  day22
  day23
  day24
  day25
  --catamain
  --gloverMain1
  --gloverMain2
  --gloverMain3
  --gloverMain4
  --gloverMain5

{-

data TreeF a = NodeF Int [a] deriving Functor
type Tree = Fix TreeF
newtype Fix f = Fix { unFix :: f (Fix f) }
type Coalgebra f a = a -> f a
type Algebra f a = f a -> a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g


alg1 :: Algebra TreeF [[Int]]
alg1 (NodeF n []) = [[n]]
alg1 (NodeF n xss) = concat $ [ (n:)<$>xs | xs<-xss]

alg2 :: Algebra TreeF [[Int]]
alg2 (NodeF n []) = [[n*n]]
alg2 (NodeF n xss) = concat $ [ (n*n:)<$>xs | xs<-xss]


coalg1 :: Coalgebra TreeF (Int, [Int])
coalg1 (x, []) = NodeF x []
coalg1 (x, xs) = NodeF x ((\y -> (y, possible y)) <$> xs)

coalg2 :: Coalgebra TreeF (Int, [Int])
coalg2 (x, []) = NodeF (x `div` 2) []
coalg2 (x, xs) = NodeF (x `div` 2) ((\y -> (y, possible y)) <$> xs)

coalg5 :: Coalgebra TreeF (Int, [Int])
coalg5 (x, []) = NodeF (x `div` 5) []
coalg5 (x, xs) = NodeF (x `div` 5) ((\y -> (y, possible y)) <$> xs)


possible 10 = [20, 30]
possible 20 = []
possible 30 = [40, 50]
possible 40 = []
possible 50 = []


testTree = Fix $ NodeF 1 [Fix $ NodeF 2 [], Fix $ NodeF 3 [Fix $ NodeF 4 [], Fix $ NodeF 5 []]]


catamain = do
  --print $ cata alg1 testTree
  --print $ cata alg2 testTree

  putStrLn $ "Note: The coalg works first to make the tree"
  putStrLn $ "and then the alg calculates from that tree\n"

  putStrLn $ "coalg1 builds the tree, alg1 just makes the lists"
  print $ hylo alg1 coalg1 (10, possible 10)
  putStrLn $ "coalg1 builds the tree, alg2 makes the lists with squares"
  print $ hylo alg2 coalg1 (10, possible 10)
  putStrLn $ "coalg2 builds the tree with n/2, alg1 just makes the lists"
  print $ hylo alg1 coalg2 (10, possible 10)
  putStrLn $ "coalg2 builds the tree with n/2, alg2 makes the lists with squares"
  print $ hylo alg2 coalg2 (10, possible 10)
  putStrLn $ "coalg5 builds the tree with n/5, alg1 just makes the lists"
  print $ hylo alg1 coalg5 (10, possible 10)
  putStrLn $ "coalg5 builds the tree with n/5, alg2 makes the lists with squares"
  print $ hylo alg2 coalg5 (10, possible 10)
-}
