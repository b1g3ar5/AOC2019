{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module Utils
    (
      toDigits
      , frequency
      ) where

import Prelude hiding (Left, Right)
import Data.List
import qualified Data.Map as M
import Data.List.Split
import Data.Map (fromListWith, toList)



frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

pad5 :: [Int] -> [Int]
pad5 xs = pad ++ xs
  where
    pad = replicate (5 - length xs) 0


-- Turns the code into 5 digits
toDigits :: Int -> [Int]
toDigits = pad5
          . reverse
          . fmap snd
          . takeWhile (/= (0, 0))
          . iterate (divMod10 . fst)
          . divMod10
  where
    divMod10 = flip divMod 10
