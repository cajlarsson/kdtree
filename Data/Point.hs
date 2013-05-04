{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts  #-}

module Data.Point (Point(..)) where

class (Ord (Elem p), Num (Elem p)) => Point p where
  
  type Elem p
  
  -- Make a point from a list of coordinates
  mkPoint :: [Elem p] -> p
  
  -- number of elements
  nDimension :: p -> Int
  -- the element type should be in Ord
  element :: Int -> p -> Elem p
  
  diff2 :: (Num (Elem p), Ord (Elem p)) => p -> p -> Int -> Elem p
  diff2 a b i = (element i a - element i b)^ (2 :: Int)

  -- |compareDistance p a b  compares the distances of a and b to p.
  compareDistance :: (Num (Elem p), Ord (Elem p)) => p -> p -> p -> Ordering
  compareDistance p a b = (dist2 p a) `compare` (dist2 p b)

  -- |dist2 returns the squared distance between two points.
  dist2 ::  (Num (Elem p), Ord (Elem p)) => p -> p -> Elem p
  dist2 a b = sum $ map (diff2 a b) [0 .. nDimension a - 1]
