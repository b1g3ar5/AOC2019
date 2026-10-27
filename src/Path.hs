module Path(
  Distance(..)
  , Direction(..)
  , Position(..)
  , Leg(..)
  , Path(..)
  , dread
  , lread
  , pread
  , manhattan
  , manhattan0
  , lcells
  , pcells
  , duration
  )where


data Direction = N | E | S | W deriving (Show, Eq)

type Distance = Int

type Position = (Int, Int)

dread :: Char -> Direction
dread c = case c of
           'U' -> N
           'R' -> E
           'D' -> S
           'L' -> W
           otherwise -> error $ "Read of non-direction: " ++ [c]

type Leg = (Direction, Distance)

lread :: String -> Leg
lread s = (dir, dist)
  where
    dir = dread $ s!!0
    dist = read $ tail s


type Path = [Leg]

pread :: String -> Path
pread s = fmap lread ls
  where
    ls = words $ map (\c->if c == ',' then ' ' else c) s


manhattan :: Position -> Position -> Distance
manhattan (x1, y1) (x2, y2) = abs (x2-x1) + abs (y2-y1)

manhattan0 :: Position -> Distance
manhattan0 p = manhattan (0, 0) p

-- Cells covered, not including initial position
lcells :: Position -> Leg -> [Position]
lcells (x, y) (dir, dist) =
  case dir of
    N -> fmap (\y->(x,y)) [(y+1)..(y+dist)]
    E -> fmap (\x->(x,y)) [(x+1)..(x+dist)]
    S -> fmap (\y->(x,y)) [(y-1), (y-2)..y-dist]
    W -> fmap (\x->(x,y)) [(x-1), (x-2)..x-dist]


-- Cells covered, not including initial position
pcells :: Position -> Path -> [Position]
pcells p ls = go [] p ls
  where
    go acc p [] = acc
    go acc p (l:ls) = go (acc ++ add) (last add) ls
      where
        add = lcells p l

duration :: Position -> [Position] -> Int
duration p qs = go 0 p qs
  where
    go t p (q:qs) = if p==q then
                      t+1
                      else
                        go (t+1) p qs
