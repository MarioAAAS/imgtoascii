# Images to Ascii Art

This is a very simple application written in Haskell that converts an Image into Ascii Art. Works better with low resolution images.

## Building

You need [GHC](https://www.haskell.org/ghc/) and [cabal-install](http://hackage.haskell.org/package/cabal-install). Then you can build with:

```
$ cabal build
```

## Usage

ImgToAscii will take an image, resize/scale it, and output Ascii Art.

You can run: `ImgToAscii --help` to have read all the vailable options:

```
Simple conversion from an Image to Ascii Art

Usage: ImgToAscii (-i|--input FILENAME) ([-r|--resize "(INT, INT)"] |
                  [-s|--scale DOUBLE] | [-w|--fix-width INT] |
                  [-h|--fix-height INT] | [-k|--keep]) ([-o|--output FILENAME] |
                  [-p|--print])
  Image to Ascii Art

Available options:
  -i,--input FILENAME      Filename of the image to convert
  -r,--resize "(INT, INT)" Resize the image into new size (width, height)
  -s,--scale DOUBLE        Scale the image by a factor
  -w,--fix-width INT       Resize the image to a width (keeping
                           ratio) (default: 80)
  -h,--fix-height INT      Resize the image to a height (keeping ratio)
  -k,--keep                Do not resize the Image
  -o,--output FILENAME     Write result to output file
  -p,--print               Write to standard output (default)
  -h,--help                Show this help text
```
