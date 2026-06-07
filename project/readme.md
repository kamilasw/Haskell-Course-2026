# Image Filters

Image-processing program that reads a PPM image, applies a pipeline of filters, and writes the result to a new PPM file.

## Supported format

The program supports plain-text `P3` PPM images.

Input images should be placed in the `images/` folder.
The output image will also appear there.


## Available filters
grayscale
invert
brighten amount
contrast amount
threshold level
blur
gaussian blur
sharpen
edge detection

## How to run

From the project root:

runghc -isrc src/Main.hs input.ppm output.ppm filter1 filter2 ...

### Examples:

runghc -isrc src/Main.hs input.ppm output_gray.ppm grayscale
runghc -isrc src/Main.hs input.ppm output_bright.ppm brighten 40
runghc -isrc src/Main.hs input.ppm output_edge.ppm grayscale edge threshold 40
runghc -isrc src/Main.hs input.ppm output_blur.ppm blur
How to run tests

From the project root:

runghc -isrc test/Spec.hs

The tests cover individual filters, pipelines, PPM read/write, and basic invariants such as preserving image dimensions and keeping pixel values in the valid range.