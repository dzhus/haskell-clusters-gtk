module Perceptron
    where

import Cluster

type Vector3 = (Double, Double, Double)
type Point3 = Vector3

-- Vector operations
(x1, y1, z1) <+> (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
(x1, y1, z1) <-> (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
(x, y, z) .> f = (x * f, y * f, z * f)
(x1, y1, z1) <.> (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

expandPoint :: Point -> Point3
expandPoint (x, y) = (x, y, 1)

-- Calculate weight vector of linear classifying function for two
-- clusters
bisect :: Cluster -> Cluster -> Double -> Vector3
bisect c1 c2 learningRate =
    let
        samples :: [(Point3, Double)]
        samples = (zip (map expandPoint (elements c1)) [1, 1 ..]) ++ 
                  (zip (map expandPoint (elements c2)) [-1, -1 ..])

        tryCorrect :: [(Point3, Double)] -> Vector3 -> Vector3
        tryCorrect ((x, sign):xs) w = if signum(x <.> w) == sign
                                      then tryCorrect xs w
                                      else tryCorrect samples (w <+> (x .> (sign * learningRate )))
        -- If all tests passed, then weight vector satisfies
        -- classifier conditions
        tryCorrect [] w = w
    in
      tryCorrect samples (0, 0, 1)
