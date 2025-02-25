module Main where

import ImageCompression (compressImage)

main :: IO ()
main = do
    -- Replace "uom.png" and "output.png" with your actual input and output file paths
    compressImage "uom.png" "output.png"