module MTape(
  Tape(..)
  , mkTape
  , moveR
  , move
  , get
  , getAt
  , setInput
  , getParams
  , getData
  , addInput
  , noInput
  , setOutput
  , delOutput
  , getAddr
  ) where


import Data.Map

import Opcode

-- We need to make this a comonad later ...
data Tape = Tape {
  focus :: Int
  , base :: Int
  , input :: [Int]
  , iix :: Int
  , output :: [Int]
  , tdata :: Map Int Int
} deriving (Show)

-- Starts with the initial tape in order
mkTape :: [Int] -> Tape
mkTape xs = Tape 0 0 [] 0 [] $ fromList $ zip [0..] xs

setInput inp (Tape f b i iix o xs) = Tape f b inp 0 o xs

setOutput out (Tape f b i iix o xs) = Tape f b i iix out xs

delOutput (Tape f b i iix o xs) = Tape f b i iix [] xs

addInput inp (Tape f b i iix o xs) = Tape f b (i++inp) iix o xs

noInput (Tape f b i iix o xs) = iix >= (length i)

moveR :: Tape -> Tape
moveR (Tape f b i ph o xs) = Tape (f + 1) b i ph o xs

moveL :: Tape -> Tape
moveL (Tape f b i ph o xs) = Tape (f - 1) b i ph o xs

move :: Int -> Tape -> Tape
move n t = (iterate moveR t)!!n

get :: Tape -> Int
get t = (tdata t)!(focus t)

getAt :: Tape -> Int -> Int
getAt t i = (tdata t)!i

getParams :: Int -> Tape -> [Int]
getParams n t = ps
  where
    ps = (\i -> get (move i t)) <$> [0..(n-1)]

getData :: Tape -> Param -> Int
--getData t (Param ADDR x) = findWithDefault 0 x (tdata t)
getData t (Param ADDR x) = (tdata t)!x
getData t (Param DATA x) = x
--getData t (Param REL x) = findWithDefault 0 (x + (base t)) (tdata t)
getData t (Param REL x) = (tdata t)!(x + (base t))

getAddr :: Tape -> Param -> Int
getAddr t (Param ADDR x) = x
getAddr t (Param DATA x) = x
getAddr t (Param REL x) = x + (base t)
