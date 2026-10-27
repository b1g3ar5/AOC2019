module Int80 (Int80, mkInt80) where

import GHC.Num
import Data.Ix

maxInt = 81

newtype Int80 = Int80 { get :: Int }
    deriving (Eq,Ord,Show)

mkInt80 :: Int -> Int80
mkInt80 x | 0 <= x && x <= maxInt = Int80 x
          | otherwise = error $ show x ++ " is not in [0, " ++ show maxInt ++ "]"

-- after this point, the Probability data constructor is not to be used

instance Num Int80 where
    Int80 x + Int80 y = mkInt80 (x + y)
    Int80 x - Int80 y = mkInt80 (x - y)
    Int80 x * Int80 y = mkInt80 (x * y)
    fromInteger = mkInt80 . fromInteger
    abs (Int80 x) = mkInt80 $ abs x
    signum  (Int80 x) = mkInt80 x

instance Ix Int80 where
  range (Int80 l, Int80 h) = mkInt80 <$> [l..h]
  index (Int80 l, Int80 h) (Int80 x) = index (l, h) x
  inRange (Int80 l, Int80 h) (Int80 x) = inRange (l, h) x


instance Bounded Int80 where
  minBound = mkInt80 0
  maxBound = mkInt80 maxInt

mx :: Int80
mx = maxBound

mxInt :: Int
mxInt = get mx

instance Enum Int80  where  
    succ (Int80 x) = mkInt80 $ x+1
    pred (Int80 x) = mkInt80 $ x-1
    toEnum = mkInt80
    fromEnum (Int80 x) = x
    enumFrom (Int80 x) = mkInt80 <$> [x..mxInt]
    enumFromThen (Int80 x) (Int80 y) = mkInt80 <$> [x,y..mxInt]  
    enumFromTo (Int80 x) (Int80 y) = mkInt80 <$> [x..y]  
    enumFromThenTo (Int80 x) (Int80 y) (Int80 z) = mkInt80 <$> [x,y..z]  
