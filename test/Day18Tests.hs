{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day18Tests(
    day18TestsA
  , day18TestsB
  , day18Props
) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Bifunctor (Bifunctor(first))
import Data.List.Split ( splitOn )
import Data.Either

import Day18 (parse)

day18Props :: TestTree
day18Props = testGroup "Day18 Props" []


day18TestsA :: TestTree
day18TestsA = testGroup "Day18 tests" $
  [ --testCase "test1"  $ (solve . preprocess . parse $ test1) @?= Right 8
  --, testCase "test2"  $ (solve . preprocess . parse $ test2) @?= Right 86
  --, testCase "test3"  $ (solve . preprocess . parse $ test3) @?= Right 132
  --, testCase "test4"  $ (solve . preprocess . parse $ test4) @?= Right 136
  --, testCase "test5"  $ (solve . preprocess . parse $ test5) @?= Right 81
  ]
  

day18TestsB :: TestTree
day18TestsB = testGroup "Day18 tests" $
  [ --testCase "test6"  $ solveB (preprocess . parse <$> test6) @?= 8
  --testCase "test7"  $ solveB (preprocess . parse <$> test7) @?= 24
  --testCase "test8"  $ solveB (preprocess . parse <$> test8) @?= 32
  --, testCase "test9"  $ solveB (preprocess . parse <$> test9) @?= 72
  ]
  


test1 = ["#########"
  , "#b.A.@.a#"
  , "#########"]

test2 = ["########################"
  , "#f.D.E.e.C.b.A.@.a.B.c.#"
  , "######################.#"
  , "#d.....................#"
  , "########################"]  

test3 = ["########################"
  , "#...............b.C.D.f#"
  , "#.######################"
  , "#.....@.a.B.c.d.A.e.F.g#"
  , "########################"]

test4 =[
    "#################"
  , "#i.G..c...e..H.p#"
  , "########.########"
  , "#j.A..b...f..D.o#"
  , "########@########"
  , "#k.E..a...g..B.n#"
  , "########.########"
  , "#l.F..d...h..C.m#"
  , "#################"]

test5 =
  [ "########################"
  , "#@..............ac.GI.b#"
  , "###d#e#f################"
  , "###A#B#C################"
  , "###g#h#i################"
  , "########################"]


test6 = [take 4 $ take 4 <$> ls, take 4 $ drop 3 <$> ls, drop 3 $ take 4 <$> ls, drop 3 $ drop 3 <$> ls]
  where
    ls = [  "#######"
          , "#a.#Cd#"
          , "##@#@##"
          , "#######"
          , "##@#@##"
          , "#cB#Ab#"
          , "#######"]




test7 = [take y $ take x <$> ls, take y $ drop (x-1) <$> ls, drop (y-1) $ take x <$> ls, drop (y-1) $ drop (x-1) <$> ls]
  where
    x = length (head ls) `div` 2 + 1
    y = length ls `div` 2 + 1
    ls = ["###############"
        , "#d.ABC.#.....a#"
        , "######@#@######"
        , "###############"
        , "######@#@######"
        , "#b.....#.....c#"
        , "###############"]

test8 = [take y $ take x <$> ls, take y $ drop (x-1) <$> ls, drop (y-1) $ take x <$> ls, drop (y-1) $ drop (x-1) <$> ls]
  where
    x = length (head ls) `div` 2 + 1
    y = length ls `div` 2 + 1
    ls = ["#############"
        , "#DcBa.#.GhKl#"
        , "#.###@#@#I###"
        , "#e#d#####j#k#"
        , "###C#@#@###J#"
        , "#fEbA.#.FgHi#"
        , "#############"]


test9 = [take y $ take x <$> ls, take y $ drop (x-1) <$> ls, drop (y-1) $ take x <$> ls, drop (y-1) $ drop (x-1) <$> ls]
  where
    x = length (head ls) `div` 2 + 1
    y = length ls `div` 2 + 1
    ls = ["#############"
        , "#g#f.D#..h#l#"
        , "#F###e#E###.#"
        , "#dCba@#@BcIJ#"
        , "#############"
        , "#nK.L@#@G...#"
        , "#M###N#H###.#"
        , "#o#m..#i#jk.#"
        , "#############"]