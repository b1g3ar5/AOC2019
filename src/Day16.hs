module Day16(
  day16
  ) where

import Data.Char
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V

parse :: String -> Vector Int
parse =  V.fromList . (digitToInt <$>)

fft :: Int -> Vector Int -> Vector Int
fft num input = V.foldl (\arr i -> step arr) input $ V.fromList [1..num]
  where
    -- pattern i replicates the code integers then repeats this pattern
    -- and drops one digit off the front
    pattern i = V.fromList $ tail . concat . repeat . concatMap (replicate i) $ [0, 1, 0, -1]

    -- step takes the input code and zips it with the row number (of the pattern
    -- matrix). This in turn is zipped with the pattern for this row and
    -- summed and then the last digit is calculated
    step :: Vector Int -> Vector Int
    step input = V.map (\(i, k) -> (abs . V.foldl (+) 0 $ V.zipWith (*) (pattern i) input) `mod` 10)
                 (V.zip (V.fromList [1..]) input)


-- The offset is 5974901 and the string is 10000*650 = 65000000
-- So we just have to deal with the matrix when the entries are 1 - so just add
-- the right bits up
partfft :: Int -> Vector Int -> Vector Int
partfft n v = iterate (V.scanr1 (\a b -> (a + b) `mod` 10)) v !! n


-- fft just does one time (ie. one matrix multiplication) and calculates the
-- resulting vector, so solveA calls fft 100 times and extracts the first
-- 8 digits
solveA :: Vector Int -> String
solveA = concatMap show . V.toList .V.take 8 . fft 100


solveB :: Vector Int -> String
solveB input = concatMap show $ V.toList $ V.take 8 $ partfft 100
             $ V.drop offset $ V.concat $ replicate 10000 input
  where
    offset = (read :: String -> Int) $ concatMap show $ V.toList $ V.take 7 input


day16 :: IO ()
day16 = do
  input <- V.fromList <$> getData

  putStrLn $ "Day16 Part1: "  ++ solveA input
  putStrLn $ "Day16 Part2: "  ++ solveB input


getData :: IO [Int]
getData = do
  txt <- readFile $ "./Data/Day16.txt"
  return $ (\c -> ord c - 48) <$> (init txt)
