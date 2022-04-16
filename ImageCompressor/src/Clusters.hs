module Clusters where

import Lib
import Vectors
import Pixels

computeClusterMean :: Cluster -> Cluster
computeClusterMean cluster@(Cluster centroid []) = cluster
computeClusterMean (Cluster _ pixels) = Cluster 
    {centroid=computePixelsMean pixels, pixels=[]}

computeClustersMean :: [Cluster] -> [Cluster]
computeClustersMean = map computeClusterMean


clusterToString :: Cluster -> String
clusterToString (Cluster centroid pixels) = "--\n" 
 ++ vector3iToString centroid ++ "\n-\n" ++ pixelsToString pixels

compressedDataToString :: [Cluster] -> IO ()
compressedDataToString [] = return ()
compressedDataToString [cluster] = putStrLn $ "\n"++clusterToString cluster
compressedDataToString (x:xs) = putStrLn (clusterToString x) >> compressedDataToString xs

