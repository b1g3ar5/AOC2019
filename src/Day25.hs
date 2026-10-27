{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}

module Day25(
  day25
  ) where


import Data.Char
import Data.List

import Tape
import Instruction


springRun :: Tape -> [String] -> Tape
springRun t prog = run $ setInput (ord <$> unlines prog) t


day25 :: IO ()
day25 = do
  t <- getTape
  tin <- getSavedInstructions
  let tout = springRun t $ tin
      ret = goItems 255 $ delOutput tout
      code = filter (`elem` ['0'..'9']) $ chr <$> output ret
  --putStrLn $ "\nPart1: " ++ (chr <$> output tout)
  --putStrLn $ "\nPart1: " ++ (chr <$> output ret)
  putStrLn $ "Day25 Part1: " ++ code
  return ()


goItems :: Int -> Tape -> Tape
goItems x t = if isWrong (chr <$> (output ts))
              then
                if x == 0
                then
                  ts
                else
                  goItems (x-1) t
              else
                ts
  where
    ts = trySet t x
    isWrong :: String -> Bool
    isWrong out = any (isInfixOf "eject") $ lines out


trySet :: Tape -> Int -> Tape
trySet t x = tout
  where
    items = toItems x
    pickUpString = foldMap (\i -> "take " ++ show i ++ "\n") items
    t1 = run $ setInput (ord <$> (pickUpString ++ "west\n")) t
    putdownString = foldMap (\i -> "drop " ++ show i ++ "\n") items
    tout = run $ setInput (ord <$> putdownString) t1


data Items = Hypercube | Cake | Ornament | FuelCell 
           | DarkMatter | KleinBottle | Hologram | EasterEgg deriving (Eq, Ord, Enum, Bounded) 


instance Show Items where
  show Hypercube = "hypercube"
  show Cake = "cake"
  show Ornament = "ornament"
  show FuelCell = "fuel cell"
  show DarkMatter = "dark matter"
  show KleinBottle = "klein bottle"
  show Hologram = "hologram"
  show EasterEgg = "easter egg"


-- converts decimal number to a list of Items
-- using the binary represenation
toItems :: Int -> [Items]
toItems n = fst <$> (filter (\(i, b) -> b == True) $ zip allItems $ go (n `mod` 256) [])
  where
    allItems :: [Items]
    allItems = [minBound..maxBound]
    go :: Int -> [Bool] -> [Bool]
    go 0 bs = reverse $ False:bs
    go 1 bs = reverse $ True:bs
    go x bs = go q $ (if r==0 then False else True):bs
      where
        (q, r) = quotRem x 2


getTape :: IO Tape
getTape = do
  txt <- readFile $ "./Data/Day25.txt"
  return $ mkTape $ read <$> words (fmap (\c->if c == ',' then ' ' else c) txt)

getSavedInstructions :: IO [String]
getSavedInstructions = do
  txt <- readFile $ "SavedTapeInstructions.txt"
  return $ lines txt

