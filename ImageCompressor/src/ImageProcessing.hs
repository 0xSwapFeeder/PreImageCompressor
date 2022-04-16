module ImageProcessing where

import Vectors

import Clusters
import Centroids
import Lib

isCentroid :: Centroid -> Cluster -> Bool
isCentroid (r1, g1, b1) (Cluster (r2, g2, b2) _) = r1 == r2 && g1 == g2 && b1 == b2

findClosestCt :: PixelUnit -> [Cluster] -> Centroid
findClosestCt p c = closest3i (extractCentroids c) (value p)

insertPixel :: PixelUnit -> Centroid -> [Cluster] -> [Cluster]
insertPixel _ _ [] = []
insertPixel pixel ct (c:cs) | isCentroid ct c = Cluster{centroid=ct, pixels=pixel:pixels c} : cs
                            | otherwise = c : insertPixel pixel ct cs

insertInClosestCluster :: PixelUnit -> [Cluster] -> [Cluster]
insertInClosestCluster pixel c = insertPixel pixel (findClosestCt pixel c) c

populateClusters :: [PixelUnit] -> [Cluster] -> [Cluster]
populateClusters [] clusters = clusters
populateClusters (pixel:rest) clusters = populateClusters rest (insertInClosestCluster pixel clusters)

kMeanIteration' :: Float -> [PixelUnit] -> [Cluster] -> [Cluster]
kMeanIteration' c pixels previous | areCentroidsConvergent previous new c = new
                                  | otherwise = kMeanIteration' c pixels new
                                    where new = populateClusters pixels (computeClustersMean previous)

kMeanIteration :: Float -> [PixelUnit] -> [Cluster] -> [Cluster]
kMeanIteration c pixels previous = 
    kMeanIteration' c pixels (populateClusters pixels (computeClustersMean previous))


kMeanCompression :: Float -> [PixelUnit] -> [Cluster] -> [Cluster]
kMeanCompression c pixels first = kMeanIteration c pixels (populateClusters pixels first)
