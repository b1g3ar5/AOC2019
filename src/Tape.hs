module Tape(
  Tape(..)
  , mkTape
  , moveR
  , moveL
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
  , isFinished
  ) where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe ( fromMaybe )

import Opcode

-- We need to make this a comonad later ...
data Tape = Tape {
  focus :: Int
  , base :: Int
  , input :: [Int]
  --, iix :: Int
  , output :: [Int]
  , memory :: Map Int Int
} deriving (Show, Eq)


-- Starts with the initial tape in order
mkTape :: [Int] -> Tape
mkTape xs = Tape 0 0 [] [] $ M.fromList $ zip [0..] xs


setInput :: [Int] -> Tape -> Tape
setInput inp (Tape f b _ o xs) = Tape f b inp o xs


setOutput :: Tape -> [Int] -> Tape
setOutput (Tape f b i _ xs) out = Tape f b i out xs


delOutput :: Tape -> Tape
delOutput (Tape f b i _ xs) = Tape f b i [] xs


addInput :: [Int] -> Tape -> Tape
addInput inp (Tape f b i o xs) = Tape f b (i++inp) o xs


noInput :: Tape -> Bool
noInput (Tape _ _ i _ _) = null i


moveR :: Tape -> Tape
moveR (Tape f b ph o xs) = Tape (f + 1) b ph o xs

moveL :: Tape -> Tape
moveL (Tape f b ph o xs) = Tape (f - 1) b ph o xs

move :: Int -> Tape -> Tape
--move n t = iterate moveR t !! n
move n t = t {focus = (focus t) + n}


get :: Tape -> Int
get t = fromMaybe 0 $ memory t M.!? focus t


isFinished :: Tape -> Bool
isFinished t = get t == 99

getAt :: Tape -> Int -> Int
getAt t i = fromMaybe 0 $ memory t M.!? i


getParams :: Int -> Tape -> [Int]
getParams n t = ps
  where
    ps = (\i -> get (move i t)) <$> [0..(n-1)]


getData :: Tape -> Param -> Int
getData t (Param ADDR x) = fromMaybe 0 $ memory t M.!? x
getData _ (Param DATA x) = x
getData t (Param REL x) = fromMaybe 0 $ memory t M.!? (x + base t)


getAddr :: Tape -> Param -> Int
getAddr _ (Param ADDR x) = x
getAddr _ (Param DATA x) = x
getAddr t (Param REL x) = x +  base t


{-

||||||||||||||||||||||||||||||||||| 
|                                 | 
|  ## #  ### ### ## #### #  ## ## | 
|    ###  # # # ##     ### ###  # | 
|   #  ### # ##  ##   ### ####### | 
|  #########  #### ### ##### ###  | 
|   # #   # ###  ###### ##  ##  # | 
| # #  # ##### ####### ## #   ##  | 
|  ## ### ##  ####  # ### #  # ## | 
| ### # ### #####   # ### ##  # # | 
| #####  ### # ###     #    ##### | 
|   ###    ## ## #### ### ##  ### | 
| ## # ### ## # ########  # #     | 
|   ### #  ###  # ## #   #  ##  # | 
|  ## # ## ##### ## ## #  # ### # | 
|   ## ## ###  ####  # # ## ##    | 
|  ### # ####    # #  # #####  #  | 
| ### ##     #### ## ## #  ##     | 
| # ##   # #  #   #  ##    #### # | 
|                                 | 
|              o                  | 
|                                 | 
|                                 | 
|- - - - - - - ----- - - - - - - -| 
|                                 | 

-}