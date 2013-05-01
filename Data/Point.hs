{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies  #-}
module Data.Point (Point(..)) where

class (Ord e, Num e) => Point p e | p -> e where
  
  -- Make a point from a list of coordinates
  mkPoint :: [e] -> p
  
  -- number of elements
  dimensions :: p -> Int
  -- the element type should be in Ord
  element :: Int -> p -> e
  
  diff2 :: p -> p -> Int -> e
  diff2 a b i = (element i a - element i b)^2

  -- |compareDistance p a b  compares the distances of a and b to p.
  compareDistance ::  p -> p -> p -> Ordering
  compareDistance p a b = dist2 p a `compare` dist2 p b

  -- |dist2 returns the squared distance between two points.
  dist2 ::  p -> p -> e
  
