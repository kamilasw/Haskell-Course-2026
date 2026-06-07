module Main where

import System.Environment (getArgs)

import Filters
import ProcessImage
import Structures

main :: IO ()
main = do
    args <- getArgs

    case args of
        inputFile : outputFile : filterArgs -> do
            let inputPath = "images/" ++ inputFile
            let outputPath = "images/" ++ outputFile
            let filters = parseFilters filterArgs

            processImage inputPath outputPath filters

            putStrLn ("Saved the result image to: " ++ outputPath)

        _ -> 
            error "usage: input.ppm output.ppm filters..."
        
parseFilters :: [String] -> [Filter]
parseFilters [] = []

parseFilters ("grayscale" : rest) =
    grayscale : parseFilters rest

parseFilters ("invert" : rest) =
    invert : parseFilters rest

parseFilters ("brighten" : amount : rest) =
    brighten (read amount) : parseFilters rest

parseFilters ("contrast" : amount : rest) =
    contrast (read amount) : parseFilters rest

parseFilters ("threshold" : level : rest) =
    threshold (read level) : parseFilters rest

parseFilters ("blur" : rest) =
    blur : parseFilters rest

parseFilters ("gaussian" : rest) =
    gaussianBlur : parseFilters rest

parseFilters ("sharpen" : rest) =
    sharpen : parseFilters rest

parseFilters ("edge" : rest) =
    edgeDetect : parseFilters rest

parseFilters (unknown : _) =
    error ("Unknown filter: " ++ unknown)