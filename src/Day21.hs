{-# LANGUAGE DeriveFunctor #-}

module Day21(
  day21
  ) where

import Data.Char (ord, chr)
import Tape (mkTape, setInput, Tape(output))
import Instruction (run, nrun)


code1, code2 :: [String]
code1 = ["NOT A J"
  , "NOT B T"
  , "OR T J"
  , "NOT C T"
  , "OR T J"
  , "AND D J"
  , "WALK"]
code2 = ["NOT A J"
    ,"NOT B T"
  , "OR T J"
  , "NOT C T"
  , "OR T J"
  , "AND D J"
  , "NOT E T"
  , "NOT T T"    -- COPY
  , "OR H T"
  , "AND T J"
  , "RUN"]


springRun :: Tape -> [String] -> Either String Int
springRun t prog = if last out > 127 then Right (last out) else Left (chr <$> out)
  where
    out = output $ nrun 1000000 $ setInput (ord <$> unlines prog) t


day21 :: IO ()
day21 = do
  t <- getTape
  putStrLn $ "Day21: Part1: " ++ show (springRun t code1)
  putStrLn $ "Day21: Part2: " ++ show (springRun t code2)
  return ()


getTape :: IO Tape
getTape = do
  txt <- readFile $ "./Data/Day21.txt"
  return $ mkTape $ read <$> words ((\c->if c == ',' then ' ' else c) <$> txt)


