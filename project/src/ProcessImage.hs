module ProcessImage (readPPM, writePPM, runPipeline, processImage) where

import Structures

runPipeline :: [Filter] -> Image -> Image
runPipeline filters image = 
    foldl (\currentImg currentFilter -> currentFilter currentImg) image filters

processImage :: FilePath -> FilePath -> [Filter] -> IO ()
processImage inputPath outputPath filters = do
    image <- readPPM inputPath
    let result = runPipeline filters image
    writePPM outputPath result

readPPM :: FilePath -> IO Image
readPPM path = do
    content <- readFile path
    let fileWords = words content

    case fileWords of
        ("P3" : w : h : maxValue : values) -> do
            let imageWidth = read w
            let imageHeight = read h
            let maxColor = read maxValue
            let imagePixels = makePixels maxColor values
            let imageRows = makeRows imageWidth imagePixels

            return Image
                { width = imageWidth
                , height = imageHeight
                , pixels = imageRows
                }

        _ ->
            error "Invalid PPM file: only P3 files are supported"


writePPM :: FilePath -> Image -> IO ()
writePPM path image = 
    writeFile path (imagetoPPM image)

makePixels :: Int -> [String] -> [Pixel]
makePixels _ [] = []

makePixels maxColor (r : g : b : rest) =
    makePixel
        (scaleColor maxColor (read r))
        (scaleColor maxColor (read g))
        (scaleColor maxColor (read b))
    : makePixels maxColor rest

makePixels _ _ =
    error "Invalid pixel data"

scaleColor :: Int -> Int -> Int
scaleColor maxColor value =
    value * 255 `div` maxColor

makeRows :: Int -> [Pixel] -> [[Pixel]]
makeRows _ [] = []
makeRows rowWidth pixelList = 
    take rowWidth pixelList : makeRows rowWidth (drop rowWidth pixelList)

imagetoPPM :: Image -> String
imagetoPPM image = 
    unlines [
        "P3",
        show (width image) ++ " " ++ show (height image),
        "255"
    ] ++
    unlines (map rowToText (pixels image))

rowToText :: [Pixel] -> String
rowToText row = 
    unwords (map pixelToText row)

pixelToText :: Pixel -> String
pixelToText pixel =
    show (red pixel) ++ " "
    ++ show (green pixel) ++ " "
    ++ show (blue pixel)

