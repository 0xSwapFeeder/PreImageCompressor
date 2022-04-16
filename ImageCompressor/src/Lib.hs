module Lib where

type PixelValue = (Int, Int, Int)

type PixelPosition = (Int, Int)

data PixelUnit = PixelUnit {
    position :: PixelPosition,
    value :: PixelValue
    } 

type Centroid = PixelValue

data Cluster = Cluster {
    centroid :: Centroid,
    pixels :: [PixelUnit]
}