module MInstruction(
  Instruction(..)
  , readInstruction
  , runInstruction
  , mkInstrution
  , run
  , nrun
  , (//)
  )where

import Data.Map hiding (null)

import Opcode
import MTape
import Computer


data Instruction = Instruction {
    itype :: OpCode
  , params :: [Param]
} deriving (Show)


readInstruction :: Tape -> (Instruction, Tape)
readInstruction tapeIn = if c == INPUT && noInput tapeIn
                         then
                           (Instruction c ps, tapeIn)
                         else
                           (Instruction c ps, tapeOut)
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

(//):: Map Int Int -> [(Int, Int)] -> Map Int Int
xs // [] = xs
xs // ((k, x):kis) = newMap // kis
  where
    newMap = insert k x xs


runInstruction :: Instruction -> Tape -> Tape
runInstruction (Instruction ADD ps) t@(Tape f b inp liix out xs) = Tape f b inp liix out (xs // [(addr, head pData + pData!!1)])
  where
    pData = getData t  <$> ps
    addr = getAddr t $ ps!!2
runInstruction (Instruction MULT ps) t@(Tape f b inp liix out xs) = Tape f b inp liix out (xs // [(addr, head pData * pData!!1)])
  where
    pData = getData t  <$> ps
    addr = getAddr t $ ps!!2
runInstruction (Instruction INPUT _) t@(Tape _ _ [] _ _ _) = t
runInstruction (Instruction INPUT ps) t@(Tape f b (inp:inps) liix out xs) = Tape f b inps liix out (xs // [(addr, inp)])
  where
    addr = getAddr t $ head ps
runInstruction (Instruction OUTPUT ps) t@(Tape f b inp liix out xs) = Tape f b inp liix (out ++ [head pData]) xs
  where
    pData = getData t  <$> ps
runInstruction (Instruction JIT ps) t@(Tape f b inp liix out xs) = Tape (if head pData == 0 then f else pData!!1) b inp liix out xs
  where
    pData = getData t  <$> ps
runInstruction (Instruction JIF ps) t@(Tape f b inp liix out xs) = Tape (if head pData == 0 then pData!!1 else f) b inp liix out xs
  where
    pData = getData t  <$> ps
runInstruction (Instruction LESSTHAN ps) t@(Tape f b inp liix out xs) = Tape f b inp liix out (xs // [(addr, if head pData < pData!!1 then 1 else 0)])
  where
    pData = getData t  <$> ps
    addr = getAddr t $ ps!!2
runInstruction (Instruction EQUALTO ps) t@(Tape f b inp liix out xs) = Tape f b inp liix out (xs // [(addr, if head pData == pData!!1 then 1 else 0)])
  where
    pData = getData t  <$> ps
    addr = getAddr t $ ps!!2
runInstruction (Instruction SETBASE ps) t@(Tape f b inp liix out xs) = Tape f (b + head pData) inp liix out xs
  where
    pData = (getData t)  <$> ps
runInstruction (Instruction HALT _) t = t


mkInstrution :: Int -> Map Int Int -> Instruction
mkInstrution lix xs =
  if xs!(lix*4) == 99 then
    Instruction HALT []
    else
      Instruction (readOpCode $ xs!(lix*4)) [Param ADDR $ xs!(lix*4+1), Param ADDR $ xs!(lix*4+2), Param ADDR $ xs!(lix*4+3)]


-- run a tape until there is no input or it halts
run :: Tape -> Tape
run tin = if (get tin == 99) || (get tin `mod` 100 == 3 && noInput tin)
            then
              tin
            else
              run $ runInstruction ins t1
  where
    (ins, t1) = readInstruction tin


nrun :: Int -> Tape -> Tape
--run 0 tin = tin
nrun n tin = if (get tin == 99) || (get tin `mod` 100 == 3 && noInput tin)
            then
              tin
            else
              nrun n $ runInstruction ins t1
  where
    (ins, t1) = readInstruction tin

