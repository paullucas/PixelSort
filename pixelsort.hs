#!/usr/bin/env stack
-- stack --resolver lts-9.21 script
import           Data.List
import           Data.List.Split
import           Graphics.GD
import           System.Environment

throw :: a
throw = errorWithoutStackTrace "Provide either a PNG or JPG file"

sortImage :: Image -> IO Image
sortImage image = do
  (width, height) <- imageSize image
  let points = [(x, y) | x <- [0 .. width], y <- [0 .. height]]
      colors = mapM (`getPixel` image) points
      render = mapM (\(point, color) -> setPixel point color image)
  _ <- render . zip points . sort =<< colors
  return image

pixelSort :: [String] -> IO ()
pixelSort [name, "png"] =
  loadPngFile (name ++ ".png") >>= sortImage >>= savePngFile "out.png"
pixelSort [name, "jpeg"] =
  loadJpegFile (name ++ ".jpeg") >>= sortImage >>= saveJpegFile 95 "out.jpeg"
pixelSort _ = throw

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filename:_) -> pixelSort $ splitOn "." filename
    _            -> throw
