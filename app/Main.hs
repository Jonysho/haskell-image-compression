module Main where

import System.Environment (getArgs)
import Quadtree (buildQuadtree)
import ImageProcessing (readImageFile, reconstructImage)
import Codec.Picture (writePng)
import System.FilePath (takeBaseName)

main :: IO ()
main = do
  args <- getArgs
  let imagePath = if null args then "image.png" else head args
  eResult <- readImageFile imagePath
  case eResult of
    Left err -> putStrLn $ "Error reading image: " ++ err
    Right (pixels, (width, height)) -> do
      let qt = buildQuadtree pixels (0, 0, width, height) 5.0 0
          outputImage = reconstructImage qt width height
      print qt
      let outputFilePath = takeBaseName imagePath ++ "-compressed.png"
      writePng outputFilePath outputImage
      putStrLn "Image compressed and saved to output.png"