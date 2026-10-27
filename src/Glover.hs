{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}


module Glover(
    gloverMain1
  , gloverMain2
  , gloverMain3
  , gloverMain4
  , gloverMain5
  ) where


import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Foldable ( hylo )
import Data.Maybe (fromMaybe)
import Control.Monad ( (<=<) )
import Control.Monad.Reader ( asks, MonadIO(liftIO), MonadReader(local), ReaderT(runReaderT) )

-- A graph is a list of nodes with a list of nodes it connects to 
gl :: [(Int, [Int])]
gl =
  [ (0, [0, 1, 4])
  , (1, [0, 2])
  , (2, [3, 4])
  , (3, [2, 1])
  , (4, [])
  ]


-- looks up in the graph
g :: Int -> [Int]
g x = fromMaybe [] $ lookup x gl

--echo :: Show a => String -> a -> IO ()
echo x y = liftIO $ putStrLn $ x ++ show y


dfs :: (Int -> [Int]) -> Int -> ReaderT [Int] IO ()
dfs graph start =
  asks (elem start) >>= \b ->
    if b
      then echo "Cycle detected involving: " start
      else do
        echo "Visiting: " start
--      Use of `local` below
--      vvvvv
        local (start:) $ do
--        Explicit recursion below
--               vvv
          mapM_ (dfs graph) $ graph start
        echo "Done with: " start


gloverMain1 :: IO ()
gloverMain1 = do
  putStrLn $ "\nStart Main1\n"
  runReaderT (dfs g 0) []
  putStrLn $ "\nEnd Main1\n"


-- I guess explicitly passing the parameter that changes might be easier to work with
dfs2 :: [Int] -> Int -> ReaderT (Int -> [Int]) IO ()
dfs2 stack start =
  if elem start stack
    then echo "Cycle detected involving: " start
    else do
      echo "Visiting: " start
--    Explicit recursion below
--           vvv
      mapM_ (dfs2 $ start:stack) =<< asks ($ start)
      echo "Done with: " start


gloverMain2 :: IO ()
gloverMain2 = do
  putStrLn $ "\nStart Main2\n"
  runReaderT (dfs2 [] (0::Int)) g
  putStrLn "\nEnd Main2\n"


dfs3 :: (Int -> [Int]) -> [Int] -> Int -> IO [Int]
dfs3 graph stack start =
  if start `elem` stack
    then echo "Cycle detected involving: " start >> return []
    else do
      echo "Visiting: " start
      results <- mapM (dfs3 graph $ start:stack) $ graph start
      echo "Done with: " start
      return $ start : concat results


hyloM :: (Traversable t, Monad m) =>
         (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM b a = b <=< mapM (hyloM b a) <=< a


g' :: (Int, [Int]) -> Compose ((,) Int) [] (Int, [Int])
g' (x, xs) = Compose $ (x,) $ if x `elem` xs then [] else map (, x:xs) $ g x


data Walk a b = Cycle a | Next a [b] deriving (Show, Functor, Foldable, Traversable)


dfsHelper :: (Int -> [Int]) -> [Int] -> Int -> Maybe [Int]
dfsHelper graph stack start =
  if start `elem` stack
    then Nothing
    else Just $ graph start


dfsHelper' :: (Int -> [Int]) -> (Int, [Int]) -> IO (Walk Int (Int, [Int]))
dfsHelper' graph (start, stack) = do
  case dfsHelper graph stack start of
    Nothing   -> do
      echo "Cycle detected involving: " start
      return $ Cycle start
    Just xs   -> do
      echo "Visiting: " start
      return $ Next start $ map (, start:stack) xs
--  maybe (return ()) (\x -> echo "Visiting: " x) ms
--  return $ Compose (ms, map (, start:stack) res)


dfsOutHelper :: Walk Int [Int] -> IO [Int]
dfsOutHelper = \case
  Cycle x   -> return []
  Next x xs -> do
    echo "Done with: " x
    return $ x : concat xs


pr :: a -> IO a
pr x = do
  -- print x
  return x


gloverMain3 :: IO ()
gloverMain3 = do
  putStrLn $ "\nStart Main3\n"
  print =<< dfs3 g [] 0
  putStrLn ""
  print =<< hyloM (dfsOutHelper <=< pr) (dfsHelper' g <=< pr) (0 :: Int, [])
  putStrLn $ "\nStart Main3\n"


g2 :: ([Int], Int) -> (Int, Maybe [([Int], Int)])
g2 (path, s) = if s `elem` path
    then (s, Nothing)
    else (s, Just $ map (s:path,) $ g s)


f2 :: (Int, Maybe [IO ()]) -> IO ()
f2 (start, foo) = case foo of
    Nothing -> echo "Cycle detected involving: " start
    Just actions -> do
        echo "Visiting: " start
        sequence_ actions
        echo "Done with: " start


gloverMain4 :: IO ()
gloverMain4 = do
  putStrLn $ "\nStart Main4\n"
  hylo (f2 . getCompose . getCompose) (Compose . Compose . g2) ([], 0)
  putStrLn "\nEnd Main4\n"


g3 :: Int -> (Int, [Int])
g3 s = (s, g s)


f3 :: (Int, [ReaderT [Int] IO ()]) -> ReaderT [Int] IO ()
f3 (start, actions) = asks (elem start) >>= \b -> if b
    then echo "Cycle detected involving: " start
    else do
        echo "Visiting: " start
        local (start:) $ sequence_ actions
        echo "Done with: " start


gloverMain5 :: IO ()
gloverMain5 = do
  putStrLn $ "\nStart Main5\n"
  runReaderT (hylo (f3 . getCompose) (Compose . g3) 0) []
  putStrLn "\nEnd Main5\n"
