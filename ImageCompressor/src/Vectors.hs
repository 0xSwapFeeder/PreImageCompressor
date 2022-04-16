module Vectors where

type Vector3f = (Float, Float, Float)

delta3f :: Vector3f -> Vector3f -> Vector3f
delta3f (x1, y1, z1) (x2, y2, z2) = (x2 - x1, y2 - y1, z2 - z1)

distance3f :: Vector3f -> Vector3f -> Float
distance3f v1 v2 = distance3f' $ delta3f v1 v2

distance3f' :: Vector3f -> Float
distance3f' (x, y, z) = sqrt x**2.0 + y**2.0 + z**2.0


type Vector3i = (Int, Int, Int)
type Vector2i = (Int, Int)

delta3i :: Vector3i -> Vector3i -> Vector3f
delta3i (x1, y1, z1) (x2, y2, z2) = (fromIntegral(x2 - x1), fromIntegral(y2 - y1), fromIntegral(z2 - z1))

distance3i :: Vector3i -> Vector3i -> Float
distance3i v1 v2 = distance3f' $ delta3i v1 v2

closest3i :: [Vector3i] -> Vector3i -> Vector3i
closest3i [] _ = (0, 0, 0)
closest3i (a:ax) toFind = closest3i' ax a toFind

closest3i' :: [Vector3i] -> Vector3i -> Vector3i -> Vector3i
closest3i' [] actual _ = actual
closest3i' (a:ax) actual toFind | distance3i a toFind < distance3i actual toFind = closest3i' ax a toFind
                                | otherwise = closest3i' ax actual toFind

sumAllDistances :: Vector3i -> [Vector3i] -> Float
sumAllDistances _ [] = 0
sumAllDistances a (x:xs) = distance3i a x + sumAllDistances a xs

removeVector3i :: Vector3i -> [Vector3i] -> [Vector3i]
removeVector3i _ [] = []
removeVector3i toRemove (x:xs) | x == toRemove = xs
                               | otherwise = x : removeVector3i toRemove xs

expelOutlier :: [Vector3i] -> [Vector3i]
expelOutlier [] = []
expelOutlier [_] = []
expelOutlier (x:xs) = removeVector3i (findOutlier (x:xs)) (x:xs)

findOutlier :: [Vector3i] -> Vector3i
findOutlier [x] = x
findOutlier [] = (0, 0, 0)
findOutlier (x:xs) = findOutlier' x (sumAllDistances x xs) (x:xs) xs


findOutlier' :: Vector3i -> Float -> [Vector3i] -> [Vector3i] -> Vector3i
findOutlier' a _ _ [] = a
findOutlier' a d list (x:xs) | d < sumAllDistances x list = findOutlier' x (sumAllDistances x list) list xs
                             | otherwise = findOutlier' a d list xs

vector3iToString :: Vector3i -> String
vector3iToString (x, y, z) = "(" ++ show x++"," ++show y++"," ++show z++")"

vector2iToString :: Vector2i -> String
vector2iToString (x, y) = "(" ++ show x++"," ++show y++")"