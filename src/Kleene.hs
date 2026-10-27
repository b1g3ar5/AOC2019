{-# LANGUAGE InstanceSigs #-}
module Kleene where

import Data.Array
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

-- See http://r6.ca/blog/20110808T035622Z.html

infixl 6 <+>
infixl 7 <.>
{- Laws:
   a <+> b = b <+> a
   (a <+> b) <+> c = a <+> (b <+> c)
   a <+> zero  = zero <+> a  = a
   (a <.> b) <.> c = a <.> (b <.> c)
   a <.> one  = one <.> a  = a
   a <.> zero = zero <.> a = zero
   a <.> (b <+> c) = a <.> b <+> a <.> c
   (a <+> b) <.> c = a <.> c <+> b <.> c
-}


kleeneExamples :: IO ()
kleeneExamples = do
  printMatrix exampleGraph
  printMatrix . star $ exampleGraph
  printMatrix . plus $ exampleGraph
  printMatrix . labelGraph $ exampleGraph
  printMatrix . reGraph . labelGraph $ exampleGraph
  printMatrix . fmap (maybe zero Tropical) $ exampleGraph2
  printMatrix . star . fmap (maybe zero Tropical) $ exampleGraph2
  printMatrix . star . annotate re $ exampleGraph2
  printMatrix . fmap (someWord . extract) . star . annotate letter $ exampleGraph2
  printMatrix . fmap (evalRE Tropical) . star . reGraph $ exampleGraph2
  printMatrix . fmap Real $ exampleMatrix
  printMatrix . star . fmap Real $ exampleMatrix
  printMatrix . inverse $ exampleMatrix

  printMatrix . star . reGraph . labelGraph $ exampleGraph

class Semiring a where
  zero  :: a
  (<+>) :: a -> a -> a
  one   :: a
  (<.>) :: a -> a -> a
  srsum :: [a] -> a
  srsum = foldr (<+>) zero
  srprod :: [a] -> a
  srprod = foldr (<.>) one

{- Laws:
   star a = one <+> a <.> star a = one <+> star a <.> a
-}
class Semiring a => StarSemiring a where
  star :: a -> a
  star a = one <+> plus a
  plus :: a -> a
  plus a = a <.> star a

{- Laws:
   a <+> a = a
   a <.> x <+> x = x  ==>  star a <.> x <+> x = x
   x <.> a <+> x = x  ==>  x <.> star a <+> x = x
-}


class StarSemiring a => KleeneAlgebra a where

data Edge i = i :-> i deriving (Eq, Ord, Bounded, Ix)

fromNode :: Edge i -> i
fromNode (x :-> _) = x
toNode :: Edge i -> i
toNode (_ :-> y) = y

newtype Matrix i e = Matrix (Array (Edge i) e)


matrix :: (Ix i, Bounded i) => (Edge i -> e) -> Matrix i e
matrix f = Matrix . listArray (minBound, maxBound) . map f $ entireRange


instance (Ix i, Bounded i, Semiring a) => Semiring (Matrix i a) where
  zero = pure zero
  (<+>) = liftA2 (<+>)
  one = matrix (\(i :-> j) -> if i == j then one else zero)
  Matrix x <.> Matrix y = matrix build
    where
      -- k needs to be all possible intermediate points, so I think indices of the map is OK?
      build (i :-> j) = srsum [x!(i :-> k) <.> y!(k :-> j) | k <- entireRange]


instance (Ix i, Bounded i, StarSemiring a) => StarSemiring (Matrix i a) where
  plus x = foldr f x entireRange
   where
    f k (Matrix m) = matrix build
     where
      build (i :-> j) = m!(i :-> j) <+> m!(i :-> k) <.> star (m!(k :-> k)) <.> m!(k :-> j)


instance (Ix i, Bounded i, KleeneAlgebra a) => KleeneAlgebra (Matrix i a) where


data Connection = Connected | Unconnected deriving Eq


type Graph i = Matrix i Connection


graph :: (Ix i, Bounded i) => [Edge i] -> Graph i
graph edgeList = matrix build
 where
  build i | i `elem` edgeList = Connected
          | otherwise         = Unconnected


data Node = A' | B' | C' | D' | E' deriving (Eq, Ord, Bounded, Ix, Show)


exampleGraph :: Graph Node
exampleGraph = graph [A' :-> B', B' :-> C', C' :-> D', C' :-> E', D' :-> B', E' :-> D']

{-
*StarSemiring> printMatrix exampleGraph
0 * 0 0 0
0 0 * 0 0
0 0 0 * *
0 * 0 0 0
0 0 0 * 0
-}

instance Semiring Connection where
  zero = Unconnected
  Connected   <+> _ = Connected
  Unconnected <+> x = x
  one = Connected
  Unconnected <.> _ = Unconnected
  Connected   <.> x = x


instance StarSemiring Connection where
  star _ = one


instance KleeneAlgebra Connection where


{-
*StarSemiring> printMatrix . star $ exampleGraph
* * * * *
0 * * * *
0 * * * *
0 * * * *
0 * * * *
*StarSemiring> printMatrix . plus $ exampleGraph
0 * * * *
0 * * * *
0 * * * *
0 * * * *
0 * * * *
-}


type LabeledGraph i = Matrix i (Maybe (Edge i))


labelGraph :: (Ix i, Bounded i) => Graph i -> LabeledGraph i
labelGraph m = f <$> m <*> matrix id
 where
  f Connected   l = Just l
  f Unconnected _ = Nothing


connect :: (Ix i) => Matrix i (Maybe a) -> Graph i
connect = fmap (maybe Unconnected (const Connected))


{-
*StarSemiring> printMatrix . labelGraph $ exampleGraph
Nothing Just (AB) Nothing   Nothing   Nothing
Nothing Nothing   Just (BC) Nothing   Nothing
Nothing Nothing   Nothing   Just (CD) Just (CE)
Nothing Just (DB) Nothing   Nothing   Nothing
Nothing Nothing   Nothing   Just (ED) Nothing
-}


data StarSemiringExpression a
  = Var a
  | Or (StarSemiringExpression a) (StarSemiringExpression a)
  | Seq (StarSemiringExpression a) (StarSemiringExpression a)
  | Star (StarSemiringExpression a)
  | None
  | Empty'
newtype RE a = RE (StarSemiringExpression a)


re :: a -> RE a
re = RE . Var


instance Eq a => Eq (RE a) where
  (RE x) == (RE y) = (todo)


instance Semiring (RE a) where
  zero :: RE a
  zero = RE None
  (<+>) :: RE a -> RE a -> RE a
  RE None <+> y = y
  x <+> RE None = x
  RE Empty' <+> RE Empty'    = RE Empty'
  RE Empty' <+> RE (Star y) = RE (Star y)
  RE (Star x) <+> RE Empty' = RE (Star x)
  RE x <+> RE y            = RE (x `Or` y)
  one :: RE a
  one = RE Empty'
  (<.>) :: RE a -> RE a -> RE a
  RE Empty' <.> y = y
  x <.> RE Empty' = x
  RE None <.> _  = RE None
  _ <.> RE None  = RE None
  RE x <.> RE y  = RE (x `Seq` y)


instance StarSemiring (RE a) where
  star (RE None)     = RE Empty'
  star (RE Empty')    = RE Empty'
  star (RE (Star x)) = star (RE x)
  star (RE x)        = RE (Star x)


instance KleeneAlgebra (RE a) where


reGraph :: (Ix i) => Matrix i (Maybe a) -> Matrix i (RE a)
reGraph = fmap (maybe zero re)
{-
*StarSemiring> printMatrix . reGraph . labelGraph $ exampleGraph
0 (AB) 0    0    0
0 0    (BC) 0    0
0 0    0    (CD) (CE)
0 (DB) 0    0    0
0 0    0    (ED) 0
-}


data Node2 = N1 | N2 | N3 | N4 | N5 | N6
             deriving (Eq, Ord, Bounded, Ix, Show)


exampleEdgeList2 :: Edge Node2 -> Maybe Integer
exampleEdgeList2 (i :-> j) = lookup (i :-> j) edges `mplus` lookup (j :-> i) edges
  where
    edges = [(N1 :-> N2, 7), (N1 :-> N3, 9), (N1 :-> N6,14)
            ,(N2 :-> N3,10), (N2 :-> N4,15)
            ,(N3 :-> N4,11), (N3 :-> N6, 2)
            ,(N4 :-> N5, 6)
            ,(N5 :-> N6, 9)
            ]


exampleGraph2 :: Matrix Node2 (Maybe Integer)
exampleGraph2 = matrix exampleEdgeList2


data Tropical a = Tropical a -- only non-negative values allowed
                | Infinity deriving (Eq, Ord)


instance (Ord a, Num a) => Semiring (Tropical a) where
  zero = Infinity
  Infinity     <+> y            = y
  x            <+> Infinity     = x
  (Tropical a) <+> (Tropical b) = Tropical (min a b)
  one = Tropical 0
  Infinity <.> _ = Infinity
  _ <.> Infinity = Infinity
  (Tropical x) <.> (Tropical y) = Tropical (x + y)


instance (Ord a, Num a) => StarSemiring (Tropical a) where
  star _ = one


instance (Ord a, Num a) => KleeneAlgebra (Tropical a) where
{-
*StarSemiring> printMatrix . fmap (maybe zero Tropical) $ exampleGraph2
∞  7  9  ∞  ∞ 14
7  ∞  10 15 ∞ ∞
9  10 ∞  11 ∞ 2
∞  15 11 ∞  6 ∞
∞  ∞  ∞  6  ∞ 9
14 ∞  2  ∞  9 ∞
*StarSemiring> printMatrix . star . fmap (maybe zero Tropical) $ exampleGraph2
0  7  9  20 20 11
7  0  10 15 21 12
9  10 0  11 11 2
20 15 11 0  6  13
20 21 11 6  0  9
11 12 2  13 9  0
-}


data ShortestPath a b = ShortestPath (Tropical a) b


instance Functor (ShortestPath a) where
  fmap f (ShortestPath a x) = ShortestPath a (f x)


extract :: ShortestPath a b -> b
extract (ShortestPath _ x) = x


instance (Ord a, Num a, Semiring b) => Semiring (ShortestPath a b) where
  zero = ShortestPath zero zero
  ShortestPath a x <+> ShortestPath b y | c < b = ShortestPath a x
                                        | c < a = ShortestPath b y
                                        | otherwise = ShortestPath c (x <+> y)
    where
      c = a <+> b
  one = ShortestPath one one
  ShortestPath a x <.> ShortestPath b y = ShortestPath (a <.> b) (x <.> y)


instance (Ord a, Num a, StarSemiring b) => StarSemiring (ShortestPath a b) where
  star (ShortestPath x b) | x == one  = ShortestPath one (star b)
                          | otherwise = ShortestPath one one


instance (Ord a, Num a, KleeneAlgebra b) => KleeneAlgebra (ShortestPath a b) where
annotate :: (Ix i, Bounded i, Ord a, Num a, Semiring b) =>
            (Edge i -> b) -> Matrix i (Maybe a) -> Matrix i (ShortestPath a b)
annotate f m = go <$> m <*> labelGraph (connect m)
 where
  go v e = ShortestPath (maybe zero Tropical v) (maybe zero f e)


{-
*StarSemiring> printMatrix . star . annotate re $ exampleGraph2
ε[0]                   (N1N2)[7]                           (N1N3)[9]        (N1N3)(N3N4)[20] (N1N3)(N3N6)(N6N5)[20]              (N1N3)(N3N6)[11]
(N2N1)[7]              ε[0]                                (N2N3)[10]       (N2N4)[15]       (N2N4)(N4N5)|(N2N3)(N3N6)(N6N5)[21] (N2N3)(N3N6)[12]
(N3N1)[9]              (N3N2)[10]                          ε[0]             (N3N4)[11]       (N3N6)(N6N5)[11]                    (N3N6)[2]
(N4N3)(N3N1)[20]       (N4N2)[15]                          (N4N3)[11]       ε[0]             (N4N5)[6]                           (N4N3)(N3N6)[13]
(N5N6)(N6N3)(N3N1)[20] (N5N4)(N4N2)|(N5N6)(N6N3)(N3N2)[21] (N5N6)(N6N3)[11] (N5N4)[6]        ε[0]                                (N5N6)[9]
(N6N3)(N3N1)[11]       (N6N3)(N3N2)[12]                    (N6N3)[2]        (N6N3)(N3N4)[13] (N6N5)[9]                           ε[0]
-}


newtype Language a = Language [[a]] deriving Show


letter x = Language [[x]]


instance Semiring (Language a) where
  zero = Language []
  (Language x) <+> (Language y) = Language (x `interleave` y)
   where
    []     `interleave` ys = ys
    (x:xs) `interleave` ys = x:(ys `interleave` xs)
  one = Language (pure [])
  (Language x) <.> (Language y) = Language (dovetail (++) x y)
   where
    dovetail f l1 l2 = concat $ go l1 (scanl (flip (:)) [] l2)
     where
      go [] _           = []
      go l1 l2@(x:y:ys) = zipWith f l1 x : go l1 (y:ys)
      go l1@(a:as) [x]  = zipWith f l1 x : go as [x]


instance StarSemiring (Language a) where
  star (Language l) = one <+> plusList (filter (not . null) l)
   where
    plusList [] = zero
    plusList l  = star (Language l) <.> Language l


instance KleeneAlgebra (Language a) where


someWord :: Language a -> Maybe [a]
someWord (Language l) = listToMaybe l
{-
*StarSemiring> printMatrix . fmap (someWord . extract) . star . annotate letter $ exampleGraph2
Just []                     Just [(N1N2)]        Just [(N1N3)]        Just [(N1N3),(N3N4)] Just [(N1N3),(N3N6),(N6N5)] Just [(N1N3),(N3N6)]
Just [(N2N1)]               Just []              Just [(N2N3)]        Just [(N2N4)]        Just [(N2N4),(N4N5)]        Just [(N2N3),(N3N6)]
Just [(N3N1)]               Just [(N3N2)]        Just []              Just [(N3N4)]        Just [(N3N6),(N6N5)]        Just [(N3N6)]
Just [(N4N3),(N3N1)]        Just [(N4N2)]        Just [(N4N3)]        Just []              Just [(N4N5)]               Just [(N4N3),(N3N6)]
Just [(N5N6),(N6N3),(N3N1)] Just [(N5N4),(N4N2)] Just [(N5N6),(N6N3)] Just [(N5N4)]        Just []                     Just [(N5N6)]
Just [(N6N3),(N3N1)]        Just [(N6N3),(N3N2)] Just [(N6N3)]        Just [(N6N3),(N3N4)] Just [(N6N5)]               Just []
-}


evalRE :: (KleeneAlgebra a) => (l -> a) -> RE l -> a
evalRE f (RE None)        = zero
evalRE f (RE Empty')       = one
evalRE f (RE (Var a))     = f a
evalRE f (RE (Star x))    = star (evalRE f (RE x))
evalRE f (RE (x `Or` y))  = evalRE f (RE x) <+> evalRE f (RE y)
evalRE f (RE (x `Seq` y)) = evalRE f (RE x) <.> evalRE f (RE y)
{-
*StarSemiring> printMatrix . fmap (evalRE Tropical) . star . reGraph $ exampleGraph2
0  7  9  20 20 11
7  0  10 15 21 12
9  10 0  11 11 2
20 15 11 0  6  13
20 21 11 6  0  9
11 12 2  13 9  0
-}


data Compact a = Real a
               | Inf


instance (Eq a, Num a) => Semiring (Compact a) where
  zero = Real 0
  Inf    <+> _      = Inf
  _      <+> Inf    = Inf
  Real x <+> Real y = Real (x + y)
  one = Real 1
  Real 0 <.> _      = Real 0
  _      <.> Real 0 = Real 0
  Inf    <.> _      = Inf
  _      <.> Inf    = Inf
  Real x <.> Real y = Real (x * y)


instance (Eq a, Fractional a) => StarSemiring (Compact a) where
  star (Real 1) = Inf
  star (Real x) = Real (recip (1 - x))
  star Inf      = Inf


exampleMatrix :: Num a => Matrix Bool a
exampleMatrix = matrix value
 where
  value (False :-> False) = 2
  value (False :-> True ) = 1
  value (True  :-> False) = 0
  value (True  :-> True ) = 2


{-
*StarSemiring> printMatrix . fmap Real $ exampleMatrix
2 1
0 2
*StarSemiring> printMatrix . star . fmap Real $ exampleMatrix
-1.0 1.0
0.0  -1.0
-}


inverse :: (Eq a, Ix i, Bounded i, Fractional a) =>
           Matrix i a -> Matrix i (Compact a)
inverse m = star (one <+> fmap (Real . negate) m)


{-
*StarSemiring> printMatrix . inverse $ exampleMatrix
0.5 -0.25
0.0 0.5
-}


instance Semiring (StarSemiringExpression a) where
  zero = None
  None <+> y = y
  x <+> None = x
  x <+> y    = x `Or` y
  one = Empty'
  Empty' <.> y     = y
  x     <.> Empty' = x
  None  <.> _     = None
  _     <.> None  = None
  x     <.> y     = x `Seq` y


instance StarSemiring (StarSemiringExpression a) where
  star None     = Empty'
  star x        = Star x


evalSSE :: (StarSemiring a) => (l -> a) -> StarSemiringExpression l -> a
evalSSE f None        = zero
evalSSE f Empty'       = one
evalSSE f (Var a)     = f a
evalSSE f (Star x)    = star (evalSSE f x)
evalSSE f (x `Or` y)  = evalSSE f x <+> evalSSE f y
evalSSE f (x `Seq` y) = evalSSE f x <.> evalSSE f y


{-
*StarSemiring> printMatrix . fmap (evalSSE Real) . star . fmap Var $ exampleMatrix
-1.0 1.0
0.0  -1.0
-}


entireRange :: (Ix i, Bounded i) => [i]
entireRange = range (minBound, maxBound)


instance (Ix i) => Functor (Matrix i) where
  fmap f (Matrix m) = Matrix (fmap f m)


instance (Ix i, Bounded i) => Applicative (Matrix i) where
  pure x = matrix (const x)
  Matrix f <*> Matrix x = matrix (\(i :-> j) -> (f!(i :-> j)) (x!(i :-> j)))


transpose :: (Ix i, Bounded i) => Matrix i a -> Matrix i a
transpose (Matrix m) = matrix (\(i :-> j) -> m!(j :-> i))


showMatrix :: (Ix i, Bounded i, Show a) => Matrix i a -> String
showMatrix (Matrix m) =   unlines [concat [pad (m'!(i :-> j)) j | j <- entireRange]
                                  | i <- entireRange]
  where
    ixs = indices m
    m' = fmap show m
    lenm = fmap length m'
    len j = maximum [lenm!(i :-> j) | i <- entireRange]
    pad s j = s ++ replicate (len j - length s + 1) ' '


printMatrix :: (Ix i, Bounded i, Show a) => Matrix i a -> IO ()
printMatrix = putStrLn . showMatrix


instance Show Connection  where
  show Connected   = "*"
  show Unconnected = "0"


instance Show a => Show (Edge a) where
  showsPrec _ (i :-> j) = showParen True (shows i . shows j)


instance Show a => Show (StarSemiringExpression a) where
  showsPrec d (Var a) = showParen (d > 10) (shows a)
  showsPrec d Empty' = showParen (d > 10) (showString "ε")
  showsPrec d None = showParen (d > 10) (showString "0")
  showsPrec d (Star x) = showParen (d > 9) (showsPrec 9 x . showString "*")
  showsPrec d (x `Or` y) = showParen (d > 6) showStr
   where
    showStr = showsPrec 6 x . showString "|" . showsPrec 6 y
  showsPrec d (x `Seq` y) = showParen (d > 7) showStr
   where
    showStr = showsPrec 7 x . showsPrec 7 y


instance Show a => Show (RE a) where
  showsPrec d (RE x) = showsPrec d x


instance Show a => Show (Tropical a) where
  show (Tropical a) = show a
  show Infinity = "∞"


instance (Show a, Show b) => Show (ShortestPath a b) where
  show (ShortestPath a x) = show x ++ "[" ++ show a ++ "]"


instance (Show a) => Show (Compact a) where
  show (Real a) = show a
  show Inf = "∞"


todo = error "TODO"
