module Opcode(
  OpCode(..)
  , readOpCode
  , ParamMode(..)
  , Param(..)
  , mkParam
  , readParamMode
  , readCode
  ) where

import Utils

-- Reads the instruction readCode
-- ABCDE : DE = opcode, abc = param modes
readCode :: Int -> (OpCode, [ParamMode])
readCode n = (readOpCode $ n `mod` 100, readParamMode <$> [n `div` 100 `mod` 10, n `div` 1000 `mod` 10, n `div` 10000 `mod` 10])
  --where
    -- readParamMode <$> [n `div` 100 `mod` 10, n `div` 1000 `mod` 10, n `div` 10000 `mod` 10]
    --xs :: [Int]
    --xs = toDigits n

{-
readCode :: Int -> (OpCode, [ParamMode])
readCode n = (readOpCode (xs!!3*10+xs!!4), readParamMode <$> [xs!!2, xs!!1, head xs])
  where
    xs :: [Int]
    xs = toDigits n


-}

data OpCode = ADD
            | MULT
            | INPUT
            | OUTPUT
            | JIT
            | JIF
            | LESSTHAN
            | EQUALTO
            | SETBASE
            | HALT deriving (Show, Eq)

readOpCode :: Int -> OpCode
readOpCode 1 = ADD -- Adds 1st and 2nd stroes in 3rd
readOpCode 2 = MULT -- Multiplies 1st and 2nd stroes in 3rd
readOpCode 3 = INPUT -- calls getInput
readOpCode 4 = OUTPUT -- sets output of Tape
readOpCode 5 = JIT -- 1st is /=0 then set focus to 2nd
readOpCode 6 = JIF -- 1st is ==0 then set focus to 2nd
readOpCode 7 = LESSTHAN -- if 1st < 2nd store 1 in 3rd else 0
readOpCode 8 = EQUALTO -- if 1st == 2nd store 1 in 3rd else 0
readOpCode 9 = SETBASE -- if 1st == 2nd store 1 in 3rd else 0
readOpCode 99 = HALT
readOpCode x = error $ "Incorrect instruction type: " ++ show x


data ParamMode = ADDR | DATA | REL deriving (Eq, Show)

readParamMode :: Int -> ParamMode
readParamMode 0 = ADDR
readParamMode 1 = DATA
readParamMode 2 = REL
readParamMode _ = error "Mode must be 0, 1 or 2"


data Param = Param {
  m :: ParamMode
  , ix :: Int} deriving (Show)

mkParam :: (ParamMode, Int) -> Param
mkParam (lm, i) = Param lm i
