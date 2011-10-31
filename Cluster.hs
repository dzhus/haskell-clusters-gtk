module Cluster
    where

type Point = (Double, Double)
data Cluster = Cluster
               {
                 center :: Point,
                 elements :: [Point],
                 threshold :: Double
               }

instance Show Cluster where
         show c = show (elements c) ++ " @ " ++ show (center c)

-- replaceIdx l i e changes i-th element in l to e. i is 1-based.
-- replace is a lie.
replaceIdx (x:xs) 0 e = e:xs
replaceIdx [] i e = []
replaceIdx (x:xs) i e = x:(replaceIdx xs (i - 1) e)

distance p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

addToCluster cluster point = cluster{elements = point:(elements cluster)}
newCluster center threshold = Cluster center [] threshold
        
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
          (addToCluster (newCluster point threshold) point):clusters
      else
      -- Add point to existing cluster
        replaceIdx clusters clusterId (addToCluster (clusters !! clusterId) point)

-- Minimum distance clusterization
clusterize :: [Point] -> Double -> [Cluster]
clusterize points threshold =
    let
        clusterize1 (p:points) clusters = clusterize1 points (classify p clusters threshold)
        clusterize1 [] clusters = clusters
    in
      clusterize1 points []
