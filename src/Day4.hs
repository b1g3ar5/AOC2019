module Day4 (
  day4
  ) where

import Data.List
import Control.Monad

day4 :: IO ()
day4 = do
  let mn = 254032
  let mx = 789860
  let ps1 = passwords1 mn mx
  putStrLn $ "Day4: Part1: Number of passwords is: " ++ show (length ps1)
  let ps2 = passwords2 mn mx
  putStrLn $ "Day4: Part2: Number of passwords is: " ++ show (length ps2)



passwords2 :: Int -> Int -> [Int]
passwords2 mn mx = do
  p1 <- [2..7]
  p2 <- [p1..9]
  p3 <- [p2..9]
  p4 <- [p3..9]
  p5 <- [p4..9]
  p6 <- [p5..9]
  let p = p1*100000 + p2*10000 + p3*1000 + p4*100 + p5*10 + p6
  guard (p > mn)
  guard (p < mx)
  guard $ ((p1==p2)&&(p2/=p3))
        || ((p1/=p2)&&(p2==p3)&&(p3/=p4))
        || ((p2/=p3)&&(p3==p4)&&(p4/=p5))
        || ((p3/=p4)&&(p4==p5)&&(p5/=p6))
        || ((p4/=p5)&&(p5==p6))
  return p


passwords1 :: Int -> Int -> [Int]
passwords1 mn mx = do
  p1 <- [2..7]
  p2 <- [p1..9]
  p3 <- [p2..9]
  p4 <- [p3..9]
  p5 <- [p4..9]
  p6 <- [p5..9]
  let p = p1*100000 + p2*10000 + p3*1000 + p4*100 + p5*10 + p6
  guard (p > mn)
  guard (p < mx)
  guard $ (p1==p2) || (p2==p3) || (p3==p4) || (p4==p5) || (p5==p6)
  return p
