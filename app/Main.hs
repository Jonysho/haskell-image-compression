module Main where

import System.Environment (getArgs)
import Quadtree (buildQuadtree)
import ImageProcessing (readImageFile, reconstructImage)
import Codec.Picture (writePng)
import System.FilePath (takeBaseName)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  
  -- Parse command line arguments
  let imagePath = if null args then "image.png" else head args
      threshold = if length args < 2 then 20.0 else read (args !! 1)
      maxDepth = if length args < 3 then 8 else read (args !! 2)
  
  putStrLn $ "Processing image: " ++ imagePath
  putStrLn $ "Threshold: " ++ show threshold
  putStrLn $ "Max depth: " ++ show maxDepth
  
  startTime <- getCurrentTime
  
  -- Read the image
  eResult <- readImageFile imagePath
  case eResult of
    Left err -> putStrLn $ "Error reading image: " ++ err
    Right (pixels, (width, height)) -> do
      putStrLn $ "Image dimensions: " ++ show width ++ "x" ++ show height
      
      -- Build the quadtree
      putStrLn "Building quadtree..."
      let bbox = (0, 0, width, height)
          qt = buildQuadtree pixels width bbox threshold maxDepth 0

      -- Create the output image
      putStrLn "Reconstructing image..."
      let outputImage = reconstructImage qt width height
      
      -- Calculate compression statistics
      midTime <- getCurrentTime
      let buildTime = diffUTCTime midTime startTime
      
      -- Save the output image
      let outputFilePath = takeBaseName imagePath ++ "-compressed.png"
      putStrLn $ "Saving to: " ++ outputFilePath
      writePng outputFilePath outputImage
      
      endTime <- getCurrentTime
      let totalTime = diffUTCTime endTime startTime
      
      putStrLn $ printf "Build time: %.2f seconds" (realToFrac buildTime :: Double)
      putStrLn $ printf "Total processing time: %.2f seconds" (realToFrac totalTime :: Double)
      putStrLn "Image compression complete!"