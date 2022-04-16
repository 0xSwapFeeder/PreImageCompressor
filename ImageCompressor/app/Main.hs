module Main where
import ImageProcessing
import Clusters
import Centroids
import VectorsFile

import Data.ByteString            as B
import Data.Text                  as T
import Data.Text.Encoding         as T
import Data.Text.IO               as T
import Prelude                    as P

import System.Exit
import Data.ByteString(readFile, unpack)
import System.Directory.Internal.Prelude
import Options.Applicative
import Data.Semigroup ((<>))

data ICargs = ICargs
  { colours      :: Int
  , convergence  :: Float
  , file :: String }


iCargs :: Parser ICargs
iCargs = ICargs
      <$> option auto
        ( short 'n' <> metavar "N" <> help "number of colors in the final image" )
      <*> option auto
        (short 'l' <> metavar "L" <> help "convergence limit" )
      <*> strOption
        (short 'f' <> metavar "F" <> help "path to the file containing the colors of the pixels" )


main :: IO ()
main = imageCompressor =<< execParser opts
  where
    opts = info (iCargs <**> helper)
      ( fullDesc
     <> progDesc "Compresses an image using k-means clustering"
      )

imageCompressor :: ICargs -> IO ()
imageCompressor (ICargs n c f) = do
    p <- fileToPixels <$> System.Directory.Internal.Prelude.readFile f
    clusters <- createClusters n
    case (clusters, p) of 
        (_, Nothing) -> exitWith (ExitFailure 84)
        (firstClusters, Just pixels) -> compressedDataToString $ kMeanCompression c pixels firstClusters

