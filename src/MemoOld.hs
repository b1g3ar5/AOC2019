{-# LANGUAGE BangPatterns #-}

module MemoOld where


import Data.Function (fix)
import Data.Hashable
import Data.Char


-- See https://stackoverflow.com/questions/3208258/memoization-in-haskell

-- For simple memoisation


cantorPairing :: Integer -> Integer -> Integer
cantorPairing a b = (a + b) * (a + b + 1) `div` 2 + a


szudzikPairing :: Integer -> Integer -> Integer
szudzikPairing a b = if a >= b then a * a + a + b else a + b * b


memof :: (Integer -> Integer) -> Integer -> Integer
memof mf 0 = 0
memof mf n = max n $ mf (n `div` 2) +
                 mf (n `div` 3) +
                 mf (n `div` 4)


data Tree a = One a | Two a a | Tree (Tree a) a (Tree a) deriving (Eq, Show)


getTree :: Tree a -> a
getTree (Tree _ x _) = x

instance Functor Tree where
  fmap f (One m) = One (f m)
  fmap f (Two m n) = Two (f m) (f n)
  fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)


index :: Tree a -> Integer -> a
index (One x) _ = x
index (Two x y) n = case (n - 1) `divMod` 2 of
 (q,0) -> y
 (q,1) -> x
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
 (q,0) -> index l q
 (q,1) -> index r q



nats :: Tree Integer
nats = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
        where
          l = n + s
          r = l + s
          s' = s * 2


finiteNats :: Integer -> Tree Integer
finiteNats limit = go 0 1
  where
    go !n !s
      | l>limit = One n
      | r>limit = Two n l
      | otherwise = Tree (go l s') n (go r s')
      where
        l = n + s
        r = l + s
        s' = s * 2


fTree :: Tree Integer
fTree = fmap (memof fastestF) nats


fastestF :: Integer -> Integer
fastestF = index fTree


-- The bool list here needs to be not padded!
-- 0 == [], 1 = True, 2 = False
rindex :: Tree a -> [Bool] -> a
rindex t bs = index t $ toInt bs


fromInt :: Integer -> [Bool]
fromInt 0 = [False]
fromInt i = helper i
  where
    helper 0 = []
    helper i = let (q,r) = i `divMod` 2 in (r==1) : helper q


pad :: Int -> a -> [a] -> [a]
pad n b bs = take n bs ++ replicate (n - length bs) b


toInt :: [Bool] -> Integer
toInt [] = 0
toInt bs = 2 * toInt (tail bs) + if head bs then 1 else 0


-- ############# Example #############
fib0 :: Integer -> Integer
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0 (n-1) + fib0 (n-2)

-- Remove the recursion
fib1 :: (Integer -> Integer) -> Integer -> Integer
fib1 f 0 = 0
fib1 f 1 = 1
fib1 f n = f (n-1) + f (n-2)

-- Build a tree
fibTree :: Tree Integer
fibTree = fmap (fib1 fastestFib) nats

-- Get the answer from the tree
fastestFib :: Integer -> Integer
fastestFib = index fibTree



