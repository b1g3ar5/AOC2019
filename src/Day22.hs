{-# LANGUAGE DeriveFunctor #-}

module Day22(
  day22
  ) where


import Data.Foldable (foldl')
import Math.NumberTheory.Moduli (SomeMod(..), modulo, getVal)
import Numeric.Natural


day22 :: IO ()
day22 = do
  txt <- readFile $ "./Data/Day22.txt"
  putStrLn $ "Day22: Part1: " ++ (solve022 txt False)
  putStrLn $ "Day22: Part2: " ++ (solve022 txt True)


-- The natural is the size of the pack
parse :: Natural -> [String] -> Shuffle
parse n ["cut", x] = Cut (read x `modulo` n)
parse n ["deal", "with", "increment", x] = Inc (read x `modulo` n)
parse n ["deal", "into", "new", "stack"] = Rev


solve022 :: String -> Bool -> String
solve022 i b = show $
  if b
  then ansB
  else ansA
  where
    times, n :: Natural
    n = if not b then 10007 else 119315717514047
    times = if not b then 1 else 101741582076661

    shuffle = parse n . words <$> lines i

    -- Could make this a monoid and then use foldMap
    ideqn = (0 `modulo` n, 1 `modulo` n)

    -- Turn shuffles into one equation
    shuffleEqn = foldl' apply ideqn shuffle

    repeatedShuffleEqn = power shuffleEqn times

    ansA = case uneval repeatedShuffleEqn 2019 of
                SomeMod k -> getVal k
                _         -> error "impossible"
    ansB = case eval repeatedShuffleEqn 2020 of
                SomeMod k -> getVal k
                _         -> error "impossible"


data Shuffle = Cut  SomeMod
             | Inc  SomeMod
             | Rev
  deriving (Show, Eq)


type Eqn = (SomeMod, SomeMod)

eval :: Eqn -> (SomeMod -> SomeMod)
eval (b, a) = \x -> a*x+b

uneval :: Eqn -> (SomeMod -> SomeMod)
uneval (b, a) = \y -> (y-b)/a



apply :: Eqn -> Shuffle -> Eqn
apply (b, i) (Cut c) = (b + c * i, i)
apply (b, i) (Inc c) = (b, i / c)
apply (b, i) Rev    = (b - i, -i)

--power :: Eqn -> SomeMod -> Eqn
power (b, a) n = (b*(1-a^n)/(1-a), a^n)



test1 = "deal with increment 7\ndeal into new stack\ndeal into new stack"
test2 = "cut 6\ndeal with increment 7\ndeal into new stack"
test3 = "deal with increment 7\ndeal with increment 9\ncut -2"
test4 = "deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1"
