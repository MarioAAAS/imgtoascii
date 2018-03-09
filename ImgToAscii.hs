module Main where
import Graphics.Image (readImageRGB, writeImage)
import Graphics.Image.Interface (Image(..), Array, toVector, toListPx, dims)
import Graphics.Image.ColorSpace (toImageY)
import Graphics.Image.Interface.Vector (VU(..))
import Graphics.Image.Processing (scale, resize, downsampleRows,
                                  Bilinear(..), Border(..))
import qualified Data.Vector.Unboxed as V
import Data.Word
import Options.Applicative
import Data.Semigroup ((<>))

-- conversion options

data Converter = Converter {inFile :: String, trans :: Tranformation,
                            out :: OutPut}
data OutPut = FileOutput FilePath | StdOut
data Tranformation = Scaling Double | Resizing (Int, Int) | FixedWidth Int |
                     FixedHeight Int | NoTrans

-- argument parsing functions

convOpts :: Parser Converter
convOpts = Converter
  <$> strOption
    ( long "input"
    <> short 'i'
    <> metavar "FILENAME"
    <> help "Filename of the image to convert" )
  <*> (resizing <|> scaling <|> fixedW <|> fixedH <|> keeping)
  <*> (fileOut <|> stdOut)

fileOut :: Parser OutPut
fileOut = FileOutput <$> strOption
  (  long "output"
  <> short 'o'
  <> metavar "FILENAME"
  <> help "Write result to output file" )

stdOut :: Parser OutPut
stdOut = flag StdOut StdOut
  ( long "print"
  <> short 'p'
  <> help "Write to standard output (default)" )

scaling :: Parser Tranformation
scaling = Scaling <$> option auto
  ( long "scale"
  <> short 's'
  <> help "Scale the image by a factor"
  <> metavar "DOUBLE" )

resizing :: Parser Tranformation
resizing = Resizing <$> option auto
  ( long "resize"
  <> short 'r'
  <> help "Resize the image into new size (width, height)"
  <> metavar "\"(INT, INT)\"" )

fixedW :: Parser Tranformation
fixedW = FixedWidth <$> option auto
  ( long "fix-width"
  <> short 'w'
  <> help "Resize the image to a width (keeping ratio)"
  <> showDefault
  <> value 80
  <> metavar "INT" )

fixedH :: Parser Tranformation
fixedH = FixedHeight <$> option auto
  ( long "fix-height"
  <> short 'h'
  <> help "Resize the image to a height (keeping ratio)"
  <> metavar "INT" )

keeping :: Parser Tranformation
keeping = flag' NoTrans
  ( long "keep"
  <> short 'k'
  <> help "Do not resize the Image" )

main :: IO ()
main = convert =<< execParser opts
  where
    opts = info (convOpts <**> helper)
      ( fullDesc
     <> progDesc "Image to Ascii Art"
     <> header "Simple conversion from an Image to Ascii Art" )

-- actual conversion functions

convert :: Converter -> IO ()
convert (Converter i t o) = do
  img <- readImageRGB VU i
  let halfGreyScale = downsampleRows . transform t $ toImageY img
      toChars = map (doubleToChar . head . toListPx) . V.toList . toVector
  writeOut o . toLines (imgWidth halfGreyScale) . toChars $ halfGreyScale

writeOut :: OutPut -> [String] -> IO ()
writeOut out = case out of
  FileOutput filePath -> writeFile "out.txt" . unlines
  _ -> mapM_ putStrLn

transform :: Array arr cs e => Tranformation -> Image arr cs e -> Image arr cs e
transform trans img = case trans of
  Scaling factor -> scale Bilinear Edge (factor, factor) img
  Resizing size -> resize Bilinear Edge size img
  FixedWidth x -> transform (Scaling (getScale x $ imgWidth img)) img
  FixedHeight y -> transform (Scaling (getScale y $ imgHeight img)) img
  _ -> img

getScale :: Int -> Int -> Double
getScale a b = fromIntegral a / fromIntegral b

imgWidth :: Array arr cs e => Image arr cs e -> Int
imgWidth = snd . dims

imgHeight :: Array arr cs e => Image arr cs e -> Int
imgHeight = fst . dims

doubleToChar :: Double -> Char
doubleToChar x = head $ drop (floor (x * 69)) cmap

cmap :: String
cmap = "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'."

toLines :: Int -> String -> [String]
toLines _ [] = []
toLines n txt = ln : toLines n lns
  where (ln, lns) = splitAt n txt
