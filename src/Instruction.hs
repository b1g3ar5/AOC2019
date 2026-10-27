module Instruction(
  Instruction(..)
  , readInstruction
  , runInstruction
  , mkInstrution
  , run
  , nrun
  --, (V.//)
  )where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Opcode
import Tape
import Debug.Trace


data Instruction = Instruction {
    itype :: OpCode
  , params :: [Param]
} deriving (Show)


readInstruction :: Tape -> (Instruction, Tape)
readInstruction tapeIn = (Instruction c ps, tapeOut)
  where
    (c, pms) = readCode $ get tapeIn
    (ps, tapeOut) = case c of
                      ADD -> (mkParam <$> zip pms (getParams 3 $ moveR tapeIn), move 4 tapeIn)
                      MULT -> (mkParam <$> zip pms (getParams 3 $ moveR tapeIn), move 4 tapeIn)
                      INPUT -> (mkParam <$> zip pms (getParams 1 $ moveR tapeIn), move 2 tapeIn)
                      OUTPUT -> (mkParam <$> zip pms (getParams 1 $ moveR tapeIn), move 2 tapeIn)
                      JIT -> (mkParam <$> zip pms (getParams 2 $ moveR tapeIn), move 3 tapeIn)
                      JIF -> (mkParam <$> zip pms (getParams 2 $ moveR tapeIn), move 3 tapeIn)
                      LESSTHAN -> (mkParam <$> zip pms (getParams 3 $ moveR tapeIn), move 4 tapeIn)
                      EQUALTO -> (mkParam <$> zip pms (getParams 3 $ moveR tapeIn), move 4 tapeIn)
                      SETBASE -> (mkParam <$> zip pms (getParams 1 $ moveR tapeIn), move 2 tapeIn)
                      HALT -> ([], tapeIn)


runInstruction :: Instruction -> Tape -> Tape
runInstruction (Instruction ADD ps) t@(Tape f b inp out xs) = Tape f b inp out $ M.insert addr (head pData + pData!!1) xs
  where
    pData = getData t <$> ps
    addr = getAddr t $ ps!!2
runInstruction (Instruction MULT ps) t@(Tape f b inp out xs) = Tape f b inp out $ M.insert addr (head pData * pData!!1) xs
  where
    pData = getData t <$> ps
    addr = getAddr t $ ps!!2
runInstruction (Instruction INPUT ps) t@(Tape f b (inp:inps) out xs) = Tape f b inps out (M.insert addr inp xs)
  where
    addr = getAddr t $ head ps
runInstruction (Instruction OUTPUT ps) t@(Tape f b inp out xs) = Tape f b inp (out ++ [head pData]) xs
  where
    pData = getData t <$> ps
runInstruction (Instruction JIT ps) t@(Tape f b inp out xs) = Tape (if head pData == 0 then f else pData!!1) b inp out xs
  where
    pData = getData t <$> ps
runInstruction (Instruction JIF ps) t@(Tape f b inp out xs) = Tape (if head pData == 0 then pData!!1 else f) b inp out xs
  where
    pData = getData t <$> ps
runInstruction (Instruction LESSTHAN ps) t@(Tape f b inp out xs) = Tape f b inp out $ M.insert addr (if head pData < pData!!1 then 1 else 0) xs
  where
    pData = getData t <$> ps
    addr = getAddr t $ ps!!2
runInstruction (Instruction EQUALTO ps) t@(Tape f b inp out xs) = Tape f b inp out $ M.insert addr (if head pData == pData!!1 then 1 else 0) xs
  where
    pData = getData t <$> ps
    addr = getAddr t $ ps!!2
runInstruction (Instruction SETBASE ps) t@(Tape f b inp out xs) = Tape f (b + head pData) inp out xs
  where
    pData = getData t <$> ps
runInstruction (Instruction HALT _) _ = undefined
runInstruction i _ = error $ "This option is not handled in runInstruction" ++ show i


mkInstrution :: Int -> Map Int Int -> Instruction
mkInstrution lix xs =
  if v4 == 99 
    then
      Instruction HALT []
    else
      Instruction (readOpCode v4) [Param ADDR v5, Param ADDR v6, Param ADDR v7]
  where
    v4 = fromMaybe 0 $ xs M.!? (lix*4)
    v5 = fromMaybe 0 $ xs M.!? (lix*4+1)
    v6 = fromMaybe 0 $ xs M.!? (lix*4+2)
    v7 = fromMaybe 0 $ xs M.!? (lix*4+3)


-- run a tape until there is no input or it halts
run :: Tape -> Tape
run tin
  | get tin == 99 = tin
  | (get tin == 3 || get tin == 203) && noInput tin = tin
  | otherwise = run $ runInstruction ins t1
  where
    (ins, t1) = readInstruction tin


nrun :: Int -> Tape -> Tape
nrun 0 tin = tin
nrun n tin = if (get tin == 99) || ((get tin == 3 || get tin == 203) && noInput tin)
              then
                tin
              else
                nrun (n-1) $ runInstruction ins t1
  where
    (ins, t1) = readInstruction tin
