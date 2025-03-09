module ImageProcessing (
    readImageFile,
    reconstructImage
) where

import Codec.Picture
import Types
import qualified Data.Vector as V

-- Read an image file and return its pixels as a Vector and dimensions
readImageFile :: FilePath -> IO (Either String (V.Vector RGB, (Int, Int)))
readImageFile path = do
  result <- readImage path
  return $ case result of
    Left err -> Left err
    Right dynImg ->
      let img = convertRGB8 dynImg
          width = imageWidth img
          height = imageHeight img
          pixels = V.generate (width * height) (\i ->
              let x = i `mod` width
                  y = i `div` width
                  PixelRGB8 r g b = pixelAt img x y
              in (r, g, b))
      in Right (pixels, (width, height))

-- Reconstruct an image from a quadtree
reconstructImage :: Quadtree -> Int -> Int -> Image PixelRGB8
reconstructImage qt width height =
  generateImage getPixel width height
  where
    getPixel :: Int -> Int -> PixelRGB8
    getPixel x y = 
      let (r, g, b) = getColor qt (x, y)
      in PixelRGB8 r g b
    
    getColor :: Quadtree -> (Int, Int) -> RGB
    getColor (Leaf (lx, ly, lw, lh) color) (x, y)
      | x >= lx && x < lx + lw && y >= ly && y < ly + lh = color
      | otherwise = (0, 0, 0)  -- Should never happen if tree covers whole image
    
    getColor (Node (nx, ny, nw, nh) tl tr bl br) (x, y)
      | x < nx || x >= nx + nw || y < ny || y >= ny + nh = (0, 0, 0)  -- Out of bounds
      | otherwise =
          let midX = nx + nw `div` 2
              midY = ny + nh `div` 2
          in if x < midX
             then if y < midY then getColor tl (x, y)
                             else getColor bl (x, y)
             else if y < midY then getColor tr (x, y)
                             else getColor br (x, y)