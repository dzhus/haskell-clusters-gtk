module Cluster
    where

import qualified Data.Set as Set

type Point = (Double, Double)

data Cluster = Cluster
               {
                 center :: Point,
                 elements :: [Point],
                 threshold :: Double
               }

instance Show Cluster where
         show c = show (elements c) ++ " @ " ++ show (center c)

instance Eq Cluster where
         (Cluster c1 e1 t1) == (Cluster c2 e2 t2) = 
                  (Set.fromList e1) == (Set.fromList e2) &&
                  c1 == c2 && 
                  t1 == t2

instance Ord Cluster where
         compare (Cluster c1 e1 t1) (Cluster c2 e2 t2)
                 -- If element sets are equal, then compare centers and thresholds
                 | (Set.fromList e1) == (Set.fromList e2) =
                   compare (compare c1 c2) (compare t2 t1)
                 | otherwise = compare e1 e2

-- replaceIdx l i e changes i-th element in l to e. i is 1-based.
-- replace is a lie.
replaceIdx (x:xs) 0 e = e:xs
replaceIdx [] i e = []
replaceIdx (x:xs) i e = x:(replaceIdx xs (i - 1) e)

distance p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

addToCluster cluster point = cluster{elements = point:(elements cluster)}

-- Make a new cluster with the same elements and threshold but new
-- center is the median point for elements
recenter :: Cluster -> Cluster
recenter cluster =
    let
        coordlists = unzip (elements cluster)
        n = length (fst coordlists)
        cx = (sum $ fst coordlists) / (fromIntegral n)
        cy = (sum $ snd coordlists) / (fromIntegral n)
    in
      Cluster (cx, cy) (elements cluster) (threshold cluster)

-- Classify a point according to minimum distance rule provided a
-- (possibly null) set of clusters.
classify :: Point -> [Cluster] -> Double -> [Cluster]
classify point clusters threshold = 
    let
        distances = zip [distance point (center c) | c <- clusters] [0..]
        -- minDist is a pair of minimal distance to cluster and its index;
        -- if no cluster is within threshold radius is found, index is -1
        minDist   = foldl (\p n -> if (fst p) <= (fst n) then p else n) (threshold, -1) distances
        clusterId = snd minDist
    in
      if clusterId == -1 then
      -- Add new cluster around this point
          (Cluster point [point] threshold):clusters
      else
      -- Add point to existing cluster
        replaceIdx clusters clusterId (addToCluster (clusters !! clusterId) point)

-- Minimum distance clustering (possibly using a predefined list of known clusters)
clusterize1 :: [Point] -> [Cluster] -> Double -> [Cluster]
clusterize1 (p:points) clusters threshold = clusterize1 points (classify p clusters threshold) threshold
clusterize1 [] clusters threshold = clusters

-- Minimum distance clustering
clusterize :: [Point] -> Double -> [Cluster]
clusterize points threshold = clusterize1 points [] threshold

-- k-means clustering
clusterizeMean points threshold maxTries =
    let
        clusterizeMean1 points clusters i =
            let
                newCenters = map center (map recenter clusters)
                oldCenters = map center clusters
                -- newClusters with only centers
                newClusters = map (\c -> Cluster c [] threshold) newCenters
            in
                -- If recentered clusters are the same as on previous
                -- or maximum step reached, give current clustering
                if (Set.fromList newCenters == Set.fromList oldCenters)
                   || (i == 0) then
                   clusters
                else
                   clusterizeMean1 points (clusterize1 points newClusters threshold) (i - 1)
    in
        clusterizeMean1 points (clusterize points threshold) maxTries
