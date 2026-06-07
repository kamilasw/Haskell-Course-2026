module Structures (Channel, Pixel(..), Image(..), Filter, minChannel, maxChannel, clampPixel, makePixel, mapPixels) where


type Channel = Int

minChannel :: Channel
minChannel = 0

maxChannel :: Channel
maxChannel = 255

data Pixel = Pixel {
    red :: Channel,
    green :: Channel,
    blue :: Channel
} deriving (Eq, Show)

data Image = Image {
    width :: Int,
    height :: Int,
    pixels :: [[Pixel]]
} deriving (Eq, Show)

type Filter = Image -> Image

clampPixel :: Int -> Channel
clampPixel value 
    | value < minChannel = minChannel
    | value > maxChannel = maxChannel
    | otherwise = value

makePixel :: Int -> Int -> Int -> Pixel
makePixel r g b =
    Pixel {
        red = clampPixel r,
        green = clampPixel g,
        blue = clampPixel b

    }

mapPixels :: (Pixel -> Pixel) -> Image -> Image
mapPixels f image =
    Image {
        width = width image,
        height = height image,
        pixels = map (map f) (pixels image)
    }
