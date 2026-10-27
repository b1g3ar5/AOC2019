
module Day1(
  day1
  )where


day1 :: IO ()
day1 = do
  ls <- getData 1

  let masses = read <$> ls :: [Int]
  putStrLn $ "Day1: Part 1: Total fuel is: " ++ show (sum $ mass2fuel1 <$> masses)
  putStrLn $ "Day1: Part 2: Total fuel is: " ++ show (sum $ mass2fuel2 <$> masses)

mass2fuel1 :: Int -> Int
mass2fuel1 m = (m `div` 3) - 2


mass2fuel2 :: Int -> Int
mass2fuel2 = go 0
  where
    go fuel mass = if mass < 9 then fuel else go (fuel + moreFuel) moreFuel
      where
        moreFuel = mass2fuel1 mass

getData :: Int -> IO [String]
getData n = do
  txt <- readFile $ "./Data/Day" ++ show n ++ ".txt"
  return $ lines txt
