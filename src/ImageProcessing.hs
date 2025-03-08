module ImageProcessing (
    readImageFile,
    reconstructImage
) where

import Codec.Picture
import Types

-- Convert an image to a 2D list of RGB pixels
imageToPixels :: Image PixelRGB8 -> [[RGB]]
imageToPixels img =
  [ [ let PixelRGB8 r g b = pixelAt img x y in (r, g, b)
    | x <- [0 .. imageWidth img - 1]
    ]
  | y <- [0 .. imageHeight img - 1]
  ]

-- Read an image file and return its pixels and dimensions
readImageFile :: FilePath -> IO (Either String ([[RGB]], (Int, Int)))
readImageFile path = do
  result <- readImage path
  return $ case result of
    Left err -> Left err
    Right dynImg ->
      let img = convertRGB8 dynImg
          pixels = imageToPixels img
      in Right (pixels, (imageWidth img, imageHeight img))

-- Reconstruct an image from a quadtree
reconstructImage :: Quadtree -> Int -> Int -> Image PixelRGB8
reconstructImage qt width height =
  generateImage (\x y -> getColor qt (x, y)) width height
  where
    getColor :: Quadtree -> (Int, Int) -> PixelRGB8
    getColor (Leaf (lx, ly, lw, lh) (r, g, b)) (x, y)
      | x >= lx && x < lx + lw && y >= ly && y < ly + lh = PixelRGB8 r g b
      | otherwise = PixelRGB8 0 0 0  -- Fallback (shouldn't happen)
    getColor (Node (nx, ny, nw, nh) tl tr bl br) (x, y) =
      let midX = nx + nw `div` 2
          midY = ny + nh `div` 2
      in if x < midX
         then if y < midY
              then getColor tl (x, y)
              else getColor bl (x, y)
         else if y < midY
              then getColor tr (x, y)
              else getColor br (x, y)