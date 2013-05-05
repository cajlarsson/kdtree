{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Trees.KdTree where
  
import Data.Point
import Data.Maybe

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Monoid as M
import Test.QuickCheck

data Point3d e = Point3d { p3x :: e, p3y :: e, p3z :: e }
    deriving (Eq, Ord, Show)

instance (Ord e, Num e) => Point (Point3d e) where
    
  type Elem (Point3d e) = e
  
  mkPoint (x:y:z:_) = Point3d x y z
  mkPoint _         = error "Not enough coordinates to make point."
  
  nDimension _ = 3

  element 0 p = p3x p
  element 1 p = p3y p
  element 2 p = p3z p


data KdTree point = KdNode { kdLeft :: KdTree point,
                             kdPoint :: point,
                             kdRight :: KdTree point,
                             kdAxis :: Int }
                  | KdEmpty
     deriving (Eq, Ord, Show)

instance Functor KdTree where
    fmap _ KdEmpty = KdEmpty
    fmap f (KdNode l x r axis) = KdNode (fmap f l) (f x) (fmap f r) axis

instance F.Foldable KdTree where
    foldr f init KdEmpty = init
    foldr f init (KdNode l x r _) = F.foldr f init3 l
        where init3 = f x init2
              init2 = F.foldr f init r


instance (Point p) =>  M.Monoid  (KdTree p) where
  mempty = KdEmpty
  mappend treeA treeB = F.foldl' (\xs x -> addPoint x xs) treeB (toList treeA)


fromList :: (Ord (Elem p), Point p) => [p] -> KdTree p
fromList points = fromListWithDepth points 0

-- |fromListWithDepth selects an axis based on depth so that the axis cycles
-- through all valid values.
fromListWithDepth :: Ord (Elem p) => Point p => [p] -> Int -> KdTree p
fromListWithDepth [] _ = KdEmpty
fromListWithDepth points depth = node
    where axis = depth `mod` nDimension (head points) 

          -- Sort point list and choose median as pivot element
          sortedPoints =
              L.sortBy (\a b -> element axis a `compare` element axis b) points
          medianIndex = length sortedPoints `div` 2
        
          -- Create node and construct subtrees
          node = KdNode { kdLeft = fromListWithDepth (take medianIndex sortedPoints) (depth+1),
                          kdPoint = sortedPoints !! medianIndex,
                          kdRight = fromListWithDepth (drop (medianIndex+1) sortedPoints) (depth+1),
                          kdAxis = axis }

-- |addPoint inserts a point into a tree
addPoint :: (Ord (Elem p), Point p) => p -> KdTree p -> KdTree p
addPoint p t = addPointWithDepth p 0 t
  where
    addPointWithDepth :: Point p => p -> Int -> KdTree p -> KdTree p
    addPointWithDepth p depth KdEmpty = KdNode { kdLeft  = KdEmpty
                                               , kdPoint = p
                                               , kdRight = KdEmpty
                                               , kdAxis  = depth `mod` nDimension p
                                               }
    addPointWithDepth p depth oldTree@(KdNode l n r axis)
      | element axis p <  element axis n  = oldTree { kdLeft = addPointWithDepth p (depth + 1) l}
      | element axis p >= element axis n     = oldTree { kdRight = addPointWithDepth p (depth + 1) r}

-- |rebalance a tree
rebalance :: (Ord (Elem p)) => Point p => KdTree p -> KdTree p
rebalance = fromList . toList


toList :: KdTree p -> [p]
toList t = F.foldr (:) [] t

-- |subtrees t returns a list containing t and all its subtrees, including the
-- empty leaf nodes.
subtrees :: KdTree p -> [KdTree p]
subtrees KdEmpty = [KdEmpty]
subtrees t@(KdNode l x r axis) = subtrees l ++ [t] ++ subtrees r

-- |nearestNeighbor tree p returns the nearest neighbor of p in tree.
nearestNeighbor :: (Ord (Elem p), Num (Elem p), Point p) => KdTree p -> p -> Maybe p
nearestNeighbor KdEmpty probe = Nothing
nearestNeighbor (KdNode KdEmpty p KdEmpty _) probe = Just p
nearestNeighbor (KdNode l p r axis) probe =
    if xProbe <= xp then findNearest l r else findNearest r l
    where xProbe = element axis probe
          xp = element axis p
          findNearest tree1 tree2 =
                let candidates1 = case nearestNeighbor tree1 probe of
                                    Nothing -> [p]
                                    Just best1 -> [best1, p]
                    sphereIntersectsPlane = (xProbe - xp)^2 <= dist2 probe p
                    candidates2 = if sphereIntersectsPlane
                                    then candidates1 ++ maybeToList (nearestNeighbor tree2 probe)
                                    else candidates1 in
                Just . L.minimumBy (compareDistance probe) $ candidates2

-- |nearNeighbors tree p returns all neighbors within distance r from p in tree.
nearNeighbors :: (Ord (Elem p), Num (Elem p), Point p) => KdTree p -> Elem p -> p -> [p]
nearNeighbors KdEmpty radius probe                      = []
nearNeighbors (KdNode KdEmpty p KdEmpty _) radius probe = if dist2 p probe <= radius^2 then [p] else []
nearNeighbors (KdNode l p r axis) radius probe          =
    if xProbe <= xp
      then let nearest = maybePivot ++ nearNeighbors l radius probe
           in if xProbe + abs radius > xp
                then nearNeighbors r radius probe ++ nearest
                else nearest
      else let nearest = maybePivot ++ nearNeighbors r radius probe
           in if xProbe - abs radius < xp
                then nearNeighbors l radius probe ++ nearest
                else nearest
  where xProbe     = element axis probe
        xp         = element axis p
        maybePivot = if dist2 probe p <= radius^2 then [p] else []

-- |isValid tells whether the K-D tree property holds for a given tree.
-- Specifically, it tests that all points in the left subtree lie to the left
-- of the plane, p is on the plane, and all points in the right subtree lie to
-- the right.
isValid :: (Ord (Elem p), Point p) => KdTree p -> Bool
isValid KdEmpty = True
isValid (KdNode l p r axis) = leftIsGood && rightIsGood
    where x = element axis p
          leftIsGood = all ((<= x) . element axis) (toList l)
          rightIsGood = all ((>= x) . element axis) (toList r)

-- |allSubtreesAreValid tells whether the K-D tree property holds for the given
-- tree and all subtrees.
allSubtreesAreValid :: (Ord (Elem p), Point p) => KdTree p -> Bool
allSubtreesAreValid = all isValid . subtrees

-- |kNearestNeighbors tree k p returns the k closest points to p within tree.
kNearestNeighbors :: (Ord (Elem p), Num (Elem p), Eq p, Point p) => KdTree p -> Int -> p -> [p]
kNearestNeighbors KdEmpty _ _ = []
kNearestNeighbors _ k _ | k <= 0 = []
kNearestNeighbors tree k probe = nearest : kNearestNeighbors tree' (k-1) probe
    where nearest = fromJust $ nearestNeighbor tree probe
          tree' = tree `remove` nearest

-- |remove t p removes the point p from t.
remove :: (Ord (Elem p), Eq p, Point p) => KdTree p -> p -> KdTree p
remove KdEmpty _ = KdEmpty
remove (KdNode l p r axis) pKill =
    if p == pKill
        then fromListWithDepth (toList l ++ toList r) axis
        else if element axis pKill <= element axis p
                then KdNode (remove l pKill) p r axis
                else KdNode l p (remove r pKill) axis


instance Arbitrary e => Arbitrary (Point3d e) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Point3d x y z)