module Day11(
  day11
  ) where


import Data.List (intercalate)
import Data.Map

import Tape
import Instruction


day11 :: IO ()
day11 = do
  tape <- getData11
  putStrLn $ "Day11: Part1: " ++ show (size $ loop Black tape)
  putStrLn $ "Day11: Part2:\n" ++ showShip (loop White tape)
  return ()


loop :: Colour -> Tape -> Ship
loop c t 
  | c == Black = go $ mkRobotBlack t
  | otherwise = go $ mkRobotWhite t
  where
    go :: Robot -> Ship
    go r@(Robot computer s _ _)
      | get computer == 99 = s
      | otherwise = go $ moveRobot r

getData11 :: IO Tape
getData11 = do
  txt <- readFile "./Data/Day11.txt"
  return $ mkTape $ read <$> words (fmap (\c->if c == ',' then ' ' else c) txt)


data Colour = Black | White deriving (Show, Eq)


decodeColor :: Int -> Colour
decodeColor 0 = Black
decodeColor 1 = White
decodeColor c = error $ "we only decode 0 (black) and 0 (white), not: " ++ show c


codeColour :: Colour -> Int
codeColour Black = 0
codeColour White = 1


writeColour :: Colour -> Char
writeColour Black = ' '
writeColour White = '#'


type Pos = (Int, Int)


data Direction = UP | DN | LF | RT deriving (Show, Eq)


-- This is a direction plus a 'turn'
(.+):: Direction -> Direction -> Direction
UP .+ LF = LF
DN .+ LF = RT
LF .+ LF = DN
RT .+ LF = UP
UP .+ RT = RT
DN .+ RT = LF
LF .+ RT = UP
RT .+ RT = DN
_ .+ y = error $ "We can only add LF or RT not: " ++ show y


decodeDirection :: Int -> Direction
decodeDirection 0 = LF
decodeDirection 1 = RT
decodeDirection c = error $ "we only decode 0 (LF) and 1 (RT), not: " ++ show c


step :: Pos -> Direction -> Pos
step (x, y) UP = (x, y+1)
step (x, y) DN = (x, y-1)
step (x, y) LF = (x-1, y)
step (x, y) RT = (x+1, y)


type Ship = Map Pos Colour


getColour :: Ship -> Pos -> Colour
getColour s p = findWithDefault Black p s


showShip :: Ship -> String
showShip m = reverse $ intercalate "\n" $ reverse . concat <$> pic
  where
    pic = (\y -> (\x->[writeColour $ getColour m (x, y)]) <$> [0..42]) <$> [-5..0]


data Robot = Robot Tape Ship Pos Direction


mkRobotBlack :: Tape -> Robot
mkRobotBlack t = Robot t empty (0, 0) UP


mkRobotWhite :: Tape -> Robot
mkRobotWhite t = Robot t (singleton (0, 0) White) (0, 0) UP


moveRobot :: Robot -> Robot
moveRobot (Robot computer s p d) = Robot newComputer2 newShip newPosition newDirection
  where
    currentColour = getColour s p
    tapeWithInput = addInput [codeColour currentColour] computer
    newComputer1 = run tapeWithInput
    out = output newComputer1
    newComputer2 = delOutput newComputer1
    newColour = if length out /= 2 then error ("Not enough output: " ++ show out) else decodeColor $ head out
    turn = decodeDirection $ out!!1
    newShip = insert p newColour s
    newDirection = d .+ turn
    newPosition = step p newDirection
