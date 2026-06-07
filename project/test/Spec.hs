import Control.Exception
import Control.Monad
import System.Directory
import System.Exit

import Filters
import ProcessImage
import Structures

type Test = (String, IO ())

main :: IO ()
main = do
    createDirectoryIfMissing True "test/tmp"

    results <- forM tests $ \(name, action) -> do
        putStr (name ++ " ... ")
        result <- try action :: IO (Either SomeException ())
        case result of
            Right _ -> do
                putStrLn "OK"
                return True

            Left err -> do
                putStrLn "FAILED"
                putStrLn ("  " ++ show err)
                return False

    unless (and results) exitFailure


tests :: [Test]
tests =
    [ ("grayscale works", testGrayscale)
    , ("invert works", testInvert)
    , ("brighten works", testBrighten)
    , ("contrast works", testContrast)
    , ("threshold works", testThreshold)
    , ("blur keeps black image black", testBlurBlackImage)
    , ("edge detection keeps constant image black", testEdgeConstantImage)
    , ("pipeline works end-to-end", testPipeline)
    , ("PPM write/read round-trip works", testPPMRoundTrip)
    , ("all filters preserve dimensions", testFiltersPreserveDimensions)
    , ("all filters keep channels in range", testFiltersKeepChannelsInRange)
    , ("grayscale is idempotent", testGrayscaleIdempotent)
    , ("threshold is idempotent", testThresholdIdempotent)
    , ("invert twice returns original image", testInvertTwice)
    ]


assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (expected == actual) $
        error $
            label ++ "\n"
            ++ "Expected: " ++ show expected ++ "\n"
            ++ "Actual:   " ++ show actual


p :: Int -> Int -> Int -> Pixel
p = makePixel


sampleImage :: Image
sampleImage =
    Image
        { width = 2
        , height = 2
        , pixels =
            [ [ p 10 20 30,    p 100 150 200 ]
            , [ p 0 255 128,   p 255 255 255 ]
            ]
        }


black3x3 :: Image
black3x3 =
    Image
        { width = 3
        , height = 3
        , pixels = replicate 3 (replicate 3 (p 0 0 0))
        }


white3x3 :: Image
white3x3 =
    Image
        { width = 3
        , height = 3
        , pixels = replicate 3 (replicate 3 (p 255 255 255))
        }


allTestImages :: [Image]
allTestImages =
    [ sampleImage
    , black3x3
    , white3x3
    ]


allFilters :: [(String, Filter)]
allFilters =
    [ ("grayscale", grayscale)
    , ("invert", invert)
    , ("brighten 20", brighten 20)
    , ("contrast 50", contrast 50)
    , ("threshold 128", threshold 128)
    , ("blur", blur)
    , ("gaussianBlur", gaussianBlur)
    , ("sharpen", sharpen)
    , ("edgeDetect", edgeDetect)
    ]


testGrayscale :: IO ()
testGrayscale =
    assertEqual
        "grayscale sample image"
        Image
            { width = 2
            , height = 2
            , pixels =
                [ [ p 20 20 20,     p 150 150 150 ]
                , [ p 127 127 127,  p 255 255 255 ]
                ]
            }
        (grayscale sampleImage)


testInvert :: IO ()
testInvert =
    assertEqual
        "invert sample image"
        Image
            { width = 2
            , height = 2
            , pixels =
                [ [ p 245 235 225,  p 155 105 55 ]
                , [ p 255 0 127,    p 0 0 0 ]
                ]
            }
        (invert sampleImage)


testBrighten :: IO ()
testBrighten =
    assertEqual
        "brighten sample image"
        Image
            { width = 2
            , height = 2
            , pixels =
                [ [ p 20 30 40,     p 110 160 210 ]
                , [ p 10 255 138,   p 255 255 255 ]
                ]
            }
        (brighten 10 sampleImage)


testContrast :: IO ()
testContrast =
    assertEqual
        "contrast one pixel"
        Image
            { width = 1
            , height = 1
            , pixels = [[p 86 161 236]]
            }
        (contrast 50 Image
            { width = 1
            , height = 1
            , pixels = [[p 100 150 200]]
            })


testThreshold :: IO ()
testThreshold =
    assertEqual
        "threshold sample image"
        Image
            { width = 2
            , height = 2
            , pixels =
                [ [ p 0 0 0,        p 255 255 255 ]
                , [ p 0 0 0,        p 255 255 255 ]
                ]
            }
        (threshold 128 sampleImage)


testBlurBlackImage :: IO ()
testBlurBlackImage =
    assertEqual
        "blur black image"
        black3x3
        (blur black3x3)


testEdgeConstantImage :: IO ()
testEdgeConstantImage =
    assertEqual
        "edge detect white image should become black"
        black3x3
        (edgeDetect white3x3)


testPipeline :: IO ()
testPipeline =
    assertEqual
        "pipeline grayscale -> threshold -> invert"
        Image
            { width = 2
            , height = 2
            , pixels =
                [ [ p 255 255 255,  p 0 0 0 ]
                , [ p 255 255 255,  p 0 0 0 ]
                ]
            }
        (runPipeline [grayscale, threshold 128, invert] sampleImage)


testPPMRoundTrip :: IO ()
testPPMRoundTrip = do
    let path = "test/tmp/roundtrip.ppm"

    writePPM path sampleImage
    imageAfterRead <- readPPM path

    assertEqual
        "image should stay the same after writePPM and readPPM"
        sampleImage
        imageAfterRead


testFiltersPreserveDimensions :: IO ()
testFiltersPreserveDimensions =
    forM_ allFilters $ \(filterName, currentFilter) ->
        forM_ allTestImages $ \image -> do
            let result = currentFilter image

            assertEqual
                (filterName ++ " should preserve width")
                (width image)
                (width result)

            assertEqual
                (filterName ++ " should preserve height")
                (height image)
                (height result)


testFiltersKeepChannelsInRange :: IO ()
testFiltersKeepChannelsInRange =
    forM_ allFilters $ \(filterName, currentFilter) ->
        forM_ allTestImages $ \image -> do
            let result = currentFilter image

            unless (all imageChannelsValid (pixels result)) $
                error (filterName ++ " produced a channel outside 0..255")


imageChannelsValid :: [Pixel] -> Bool
imageChannelsValid row =
    all pixelChannelsValid row


pixelChannelsValid :: Pixel -> Bool
pixelChannelsValid pixel =
    channelValid (red pixel)
    && channelValid (green pixel)
    && channelValid (blue pixel)


channelValid :: Int -> Bool
channelValid value =
    value >= 0 && value <= 255


testGrayscaleIdempotent :: IO ()
testGrayscaleIdempotent =
    assertEqual
        "grayscale . grayscale == grayscale"
        (grayscale sampleImage)
        (grayscale (grayscale sampleImage))


testThresholdIdempotent :: IO ()
testThresholdIdempotent =
    assertEqual
        "threshold . threshold == threshold"
        (threshold 128 sampleImage)
        (threshold 128 (threshold 128 sampleImage))


testInvertTwice :: IO ()
testInvertTwice =
    assertEqual
        "invert . invert == id"
        sampleImage
        (invert (invert sampleImage))