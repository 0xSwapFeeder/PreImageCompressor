module Centroids where

import Lib
import Vectors
import Data.Functor

import System.Random

isCentroidConvergent :: Centroid -> Centroid -> Float -> Bool
isCentroidConvergent new previous convergence 
 = distance3i new previous <= convergence

areCentroidsConvergent :: [Cluster] -> [Cluster] -> Float -> Bool
areCentroidsConvergent _ [] _ = True
areCentroidsConvergent [] _ _ = True
areCentroidsConvergent (new:ns) (previous:ps) convergence
    | not $ isCentroidConvergent (centroid new) (centroid previous) convergence = False
    | otherwise = areCentroidsConvergent ns ps convergence

extractCentroids :: [Cluster] -> [Centroid]
extractCentroids [] = []
extractCentroids clusters = map centroid clusters


createRandomCentroid :: IO Centroid
createRandomCentroid = do
    r <- randomRIO (0, 255)
    g <- randomRIO (0, 255)
    b <- randomRIO (0, 255)
    return (r, g, b)

createIOCluster :: IO Cluster 
createIOCluster = do
    centroid <- createRandomCentroid
    return $ Cluster centroid []

createIOClusters :: Int -> [IO Cluster]
createIOClusters 0 = []
createIOClusters n = createIOCluster : createIOClusters (n-1)

convertClusters :: [IO Cluster] -> IO [Cluster]
convertClusters [] = return []
convertClusters (cluster:clusters) = do
    c <- cluster
    cs <- convertClusters clusters
    return $ c : cs

createClusters :: Int -> IO [Cluster]
createClusters n = convertClusters $ createIOClusters n