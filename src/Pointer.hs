{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Pointer
  ( 
  -- * The Pointer comonad
    Pointer, pointer, runPointer
  -- * The Pointer comonad transformer
  , pointerBounds 
  , module Control.Comonad.Store.Class
  ) where

import Control.Comonad
import Control.Comonad.Store.Class 
import Data.Array


data Pointer i a = Pointer (Array i a) i


pointer :: Array i a -> i -> Pointer i a 
pointer = Pointer


runPointer :: Pointer i a -> (Array i a, i)
runPointer (Pointer f i) = (f, i)


instance (Ix i) => Functor (Pointer i) where
  fmap f (Pointer arr ix) = Pointer (f <$> arr) ix


instance (Ix i) => Comonad (Pointer i) where
  extract (Pointer g i) = g ! i 
  duplicate (Pointer g i) = Pointer (go g) i 
    where
      go x = listArray b $ Pointer x <$> range b 
        where 
          b = bounds x 


--instance (Ix i) => ComonadApply (Pointer i) where


instance (Ix i) => ComonadStore i (Pointer i) where
  pos (Pointer _ i) = i
  seek i ~(Pointer g _) = Pointer g i
  seeks f ~(Pointer g i) = Pointer g (f i)
  peek i (Pointer g _) = g ! i 
  peeks f ~(Pointer g i) = g ! f i


-- | Extract the bounds of the currently focused array
pointerBounds :: (Ix i) => Pointer i a -> (i,i)
pointerBounds (Pointer g _) = bounds g


