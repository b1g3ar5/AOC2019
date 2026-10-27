{-# language DeriveFunctor #-}
{-# language TupleSections #-}

module Tree(
  treeMain
  )where


import qualified Data.Map as M hiding (findIndex)
import Data.List
import Data.List.Split
import Data.Maybe

-- Recursion library
newtype Fix f = Fix { unFix :: f (Fix f) }
type Coalgebra f a = a -> f a
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
type Algebra f a = f a -> a
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g


-- Functor that generates the rose tree
data TreeF a = NodeF String [a]
  deriving (Functor, Show)

-- Rose tree
type Tree = Fix TreeF

-- A mulitmap: Who orbits a given name (directly)
type Pool = M.Map String [String]

-- Unfolding the tree from the pool
mkNode :: Coalgebra TreeF (String, Pool)
mkNode (key, pool) =
  case M.lookup key pool of
    Nothing  -> NodeF key []  -- Leaf
    Just lst -> -- list of children
      let newPool = M.delete key pool
      in NodeF key (fmap ( , newPool) lst) -- tuple section


-- Accumulates (#of children, # of orbits)
-- Accumulates (#of children, # of orbits)
orbits :: Algebra TreeF (Int, Int)
orbits (NodeF _ lst) = (childCount, orbitCount + childCount)
  where
    childCount = sum $ fmap ((+1) . fst) lst
    orbitCount = sum $ fmap snd lst



allOrbits pool = snd $ hylo orbits mkNode ("COM", pool)

-- The accumulator
data Accum = Acc { seenMe :: Bool
                 , seenSa :: Bool
                 , dist  :: Int } deriving Show


-- Combine info gathered from children
meAndSanta :: Algebra TreeF Accum
meAndSanta (NodeF name accums) =
    if name == "YOU" || name == "SAN"
    then
      if name == "YOU"
      then if hasSanta accums
           then Acc True  True (distToSa accums)
           else Acc True  False 0
      else if hasMe accums
           then Acc True  True (distToMe accums)
           else Acc False True 0
    else -- current node is neither me nor santa.
         -- Maybe children have seen either?
      let ixMe = findIndex seenMe accums
          ixSa = findIndex seenSa accums
      in case (ixMe, ixSa) of
         (Nothing, Nothing)     -> Acc False False (-1)
         (Nothing, Just idx2)   -> Acc False True  (1 + dist (accums!! idx2))
         (Just idx1, Nothing)   -> Acc True  False (1 + dist (accums!! idx1))
         (Just idx1, Just idx2) -> -- Both have been seen!
             if idx1 == idx2  -- in the same child tree
             then Acc True True ( dist (accums!! idx1))
             else Acc True True ((dist (accums!! idx1)) + (dist (accums!! idx2)))
  where hasMe    = or . fmap seenMe
        hasSanta = or . fmap seenSa
        distToMe = dist . fromJust . find seenMe
        distToSa = dist . fromJust . find seenSa


meetSanta pool = dist $ hylo meAndSanta mkNode ("COM", pool)

-- Testing ....
allOrbitsFromTree = cata orbits
meetingFromTree = cata meAndSanta

showTree :: Tree -> String
showTree t = cata showNode t
  where
    showNode :: Algebra TreeF String
    showNode (NodeF s lst) = s ++ "->[" ++ intercalate "," lst ++ "]"

drawTree :: Tree -> String
drawTree t = unlines $ draw t

draw :: Tree -> [String]
draw (Fix (NodeF x ts0)) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)


-- Add item to the pool
appendMulti :: (String, String) -> Pool -> Pool
appendMulti (key, value) pool =
  case M.lookup key pool of
    Nothing -> M.insert key [value] pool
    Just lst -> M.insert key (value:lst) pool

toPair :: [a] -> (a, a)
toPair (a:b:[]) = (a, b)

treeMain = do
  --text <- readFile "Day6.txt"
  let text = unlines $ test3
  let pairs = fmap (toPair . splitOn ")") $ lines text
  --print pairs
  let pool = Prelude.foldl (\m kv -> appendMulti kv m) M.empty pairs
  --print pool
  let tree = ana mkNode ("COM", pool)
  putStrLn $ showTree tree
  putStrLn $ drawTree tree
  print $ allOrbits pool
  print $ meetSanta   pool



test1 = ["COM)B"]
test2 = ["COM)B", "B)C", "B)D"]
test3 = ["COM)B"
       , "B)C"
       , "C)D"
       , "D)E"
       , "E)F"
       , "B)G"
       , "G)H"
       , "D)I"
       , "E)J"
       , "J)K"
       , "K)L"]

test4 = ["COM)B"
        ,"B)C"
        ,"C)D"
        ,"D)E"
        ,"E)F"
        ,"B)G"
        ,"G)H"
        ,"D)I"
        ,"E)J"
        ,"J)K"
        ,"K)L"
        ,"K)YOU"
        ,"I)SAN"]
