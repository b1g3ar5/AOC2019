{-# LANGUAGE DeriveFunctor #-}

module Day19(
  day19
  ) where

import Data.List (intercalate)
import Tape (mkTape, setInput, Tape(output))
import Instruction (run, nrun)


day19 :: IO ()
day19 = do
  t <- getTape
  let mp n = getMap n t
  --putStrLn $ "\nDay19: Part1: \n" ++ mp 49
  
  let counts :: [Int]
      counts = sum . ((\c -> if c== '#' then 1 else 0) <$>) <$> lines (mp 49)

  let y = 695
  putStrLn $ "Day19: Part1: " ++ show (sum counts)
  putStrLn $ "Day19: Part2: " ++ show (boxSize 695 t)
  return ()



boxSize :: Int -> Tape -> Maybe Int
boxSize y t -- = Just $ se - es + 1
  | (se - es + 1) >= 100 = Just $ y * 10000 + es
  | otherwise = Nothing
  where
    (_, se, _) = lineLength y t
    (es, _, _) = lineLength (y+99) t



lineLength :: Int -> Tape -> (Int, Int, Int)
lineLength n t = (goStart 0, goEnd 0, goEnd 0 - goStart 0 + 1)
  where
    goStart i
      | v==0 && w==1 = i+1
      | v==0 && w==0 = goStart (i+1)
      | otherwise = error "This can't happen in goStart"
      where
        v = head $ output $ nrun 1000 $ setInput [n, i] t
        w = head $ output $ nrun 1000 $ setInput [n, i+1] t
    goEnd i
      | v==0 = goEnd (i+1)
      | v==1 && w==1 = goEnd (i+1)
      | v==1 && w==0 = i
      | otherwise = error "This can't happen in goEnd"
      where
        v = head $ output $ nrun 1000 $ setInput [n, i] t
        w = head $ output $ nrun 1000 $ setInput [n, i+1] t



getMap :: Int -> Tape -> String
getMap n t = intercalate "\n" $ (\x -> go x <$> [0..n]) <$> [0..n]
  where
    go x y = toChar $ head $ output $ nrun 1000 $ setInput [x, y] t
    toChar 0 = '.'
    toChar 1 = '#'
    toChar _ = error "Must be 0 or 1"


getTape :: IO Tape
getTape = do
  txt <- readFile $ "./Data/Day19.txt"
  return $ mkTape $ read <$> words (fmap (\c->if c == ',' then ' ' else c) txt)
