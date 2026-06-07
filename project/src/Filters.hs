module Filters (grayscale, invert, brighten, threshold, contrast, blur, gaussianBlur, sharpen, edgeDetect) where

import Structures

grayscale :: Filter
grayscale =
    mapPixels grayscalePixel


grayscalePixel :: Pixel -> Pixel
grayscalePixel pixel =
    let average = (red pixel + green pixel + blue pixel) `div` 3
    in makePixel average average average


invert :: Filter
invert =
    mapPixels invertPixel


invertPixel :: Pixel -> Pixel
invertPixel pixel =
    makePixel
        (maxChannel - red pixel)
        (maxChannel - green pixel)
        (maxChannel - blue pixel)


brighten :: Int -> Filter
brighten amount =
    mapPixels (brightenPixel amount)


brightenPixel :: Int -> Pixel -> Pixel
brightenPixel amount pixel =
    makePixel
        (red pixel + amount)
        (green pixel + amount)
        (blue pixel + amount)


contrast :: Int -> Filter
contrast amount =
    mapPixels (contrastPixel amount)


contrastPixel :: Int -> Pixel -> Pixel
contrastPixel amount pixel =
    makePixel
        (changeContrast amount (red pixel))
        (changeContrast amount (green pixel))
        (changeContrast amount (blue pixel))


changeContrast :: Int -> Int -> Int
changeContrast amount value =
    128 + ((value - 128) * (100 + amount)) `div` 100


threshold :: Int -> Filter
threshold level =
    mapPixels (thresholdPixel level)


thresholdPixel :: Int -> Pixel -> Pixel
thresholdPixel level pixel =
    let average = (red pixel + green pixel + blue pixel) `div` 3
        fixedLevel = clampPixel level
    in if average >= fixedLevel
        then makePixel maxChannel maxChannel maxChannel
        else makePixel minChannel minChannel minChannel


type Kernel = [[Int]]


blur :: Filter
blur =
    convolve3x3
        [ [1, 1, 1]
        , [1, 1, 1]
        , [1, 1, 1]
        ]
        9


gaussianBlur :: Filter
gaussianBlur =
    convolve3x3
        [ [1, 2, 1]
        , [2, 4, 2]
        , [1, 2, 1]
        ]
        16


sharpen :: Filter
sharpen =
    convolve3x3
        [ [ 0, -1,  0]
        , [-1,  5, -1]
        , [ 0, -1,  0]
        ]
        1


edgeDetect :: Filter
edgeDetect =
    convolve3x3
        [ [-1, -1, -1]
        , [-1,  8, -1]
        , [-1, -1, -1]
        ]
        1


convolve3x3 :: Kernel -> Int -> Filter
convolve3x3 kernel divisor image =
    Image
        { width = width image
        , height = height image
        , pixels =
            [ [ convolvePixel image kernel divisor x y
              | x <- [0 .. width image - 1]
              ]
            | y <- [0 .. height image - 1]
            ]
        }

convolvePixel :: Image -> Kernel -> Int -> Int -> Int -> Pixel
convolvePixel image kernel divisor x y =
    let positions = [-1, 0, 1]

        weightedPixels =
            [ (kernelValue kernel dx dy, getPixel image (x + dx) (y + dy))
            | dy <- positions
            , dx <- positions
            ]

        newRed = sum [ weight * red pixel | (weight, pixel) <- weightedPixels ] `div` divisor
        newGreen = sum [ weight * green pixel | (weight, pixel) <- weightedPixels ] `div` divisor
        newBlue = sum [ weight * blue pixel | (weight, pixel) <- weightedPixels ] `div` divisor

    in makePixel newRed newGreen newBlue


kernelValue :: Kernel -> Int -> Int -> Int
kernelValue kernel dx dy =
    kernel !! (dy + 1) !! (dx + 1)


getPixel :: Image -> Int -> Int -> Pixel
getPixel image x y =
    let safeX = clampIndex x (width image - 1)
        safeY = clampIndex y (height image - 1)
    in pixels image !! safeY !! safeX


clampIndex :: Int -> Int -> Int
clampIndex value maxIndex
    | value < 0 = 0
    | value > maxIndex = maxIndex
    | otherwise = value