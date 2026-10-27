module Point where


import           Text.Printf


data Point = Point { py :: {-# UNPACK #-} !Int
                   , px :: {-# UNPACK #-} !Int
                   } deriving (Eq, Ord)


instance Show Point where
  show (Point y x) = printf "<y:%d,x:%d>" y x


type Vector = Point


instance Num Point where
  (Point y1 x1) + (Point y2 x2) = Point (y1 + y2) (x1 + x2)
  (Point y1 x1) - (Point y2 x2) = Point (y1 - y2) (x1 - x2)
  abs    (Point y x) = Point (abs    y) (abs    x)
  negate (Point y x) = Point (negate y) (negate x)
  signum (Point y x) = Point (signum y) (signum x)
  fromInteger _ = error "can't implictly convert int to vector"
  (*) = error "tried to multiply two 2D vectors"



-- cardinal coordinates

data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Show, Eq, Bounded, Enum)

directionVector :: Direction -> Vector
directionVector cd = case cd of
                      N  -> Point u 0
                      NE -> Point u r
                      E  -> Point 0 r
                      SE -> Point d r
                      S  -> Point d 0
                      SW -> Point d l
                      W  -> Point 0 l
                      NW -> Point u l
  where u = -1
        d =  1
        l = -1
        r =  1