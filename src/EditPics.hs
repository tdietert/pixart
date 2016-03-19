module EditPics
    ( editPixelsPng
    ) where

import Data.List
import Data.List.Split

import Codec.Picture
import Codec.Picture.Png
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V

vChunksOf :: (V.Storable a) => Int -> V.Vector a -> [V.Vector a]
vChunksOf n = map V.fromList . chunksOf n . V.toList

transposePng :: Image PixelRGB8 -> Image PixelRGB8
transposePng (Image imgW imgH imgData) =
    Image imgH imgW $ V.concat $ map V.concat $ transpose rowsOfPixels
  where
    rowsOfPixels :: [[V.Vector (PixelBaseComponent PixelRGB8)]]
    rowsOfPixels = chunksOf imgW $ vChunksOf 3 imgData

swapEveryOther :: [a] -> [a]
swapEveryOther (w:x:y:z:zs) = x:w:y:z : swapEveryOther zs
swapEveryOther lst = lst

recSwapVert :: [a] -> [a]
recSwapVert [] = []
recSwapVert [x] = [x]
recSwapVert xs = back ++ recSwapVert front
  where
    (front,back) = splitAt (length xs `div` 2) xs

editPixelsPng :: DynamicImage -> Either String DynamicImage
editPixelsPng (ImageRGB8 png) = Right . ImageRGB8 $
    transposePng $ png { imageData = V.concat (front ++ recSwapVert back) }
  where
    (Image imgW imgH imgData) = transposePng png
    chunks = vChunksOf (imgW*3) imgData
    (front,back) = splitAt (length chunks `div` 2) chunks
