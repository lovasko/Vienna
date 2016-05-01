module Font
( Font(..)
, Glyph(..)
, fontLoad
, fontGetGlyph
) where

import qualified Data.Map as MP
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits (testBit)
import Data.Char (ord, chr)
import Data.List (intersperse)

data Glyph = Glyph Int BS.ByteString
type Font = MP.Map Char Glyph 

-- ASCII art representation of the glyph.
instance Show Glyph where
  show (Glyph w bytes) = concat $ intersperse "\n" (map row2string rows)
    where
      -- Split the glyph into rows.
      rows :: [BS.ByteString]
      rows = splitEvery w bytes 

      -- Convert the row into ASCII art.
      row2string :: BS.ByteString -> String
      row2string row = concatMap byte2string (BS.unpack row)

      -- Convert a byte into eight ASCII characters based on zeros and ones.
      byte2string :: Word8 -> String
      byte2string byte = concatMap (bool2string . testBit byte) [7,6..0]

      -- Define the ASCII art glpyhs.
      bool2string :: Bool -> String
      bool2string True = "#"
      bool2string False = "."

-- Split the ByteString into a list of smaller ByteStrings.
splitEvery :: Int             -- size of each new sub-ByteString
           -> BS.ByteString   -- ByteString to split
           -> [BS.ByteString] -- list of sub-ByteStrings
splitEvery n bs
  | n <= 0 || BS.null bs = []
  | otherwise = BS.take n bs:splitEvery n (BS.drop n bs)

-- Load an alphabet from the ByteString data.
loadAlphabet :: Char             -- starting letter of the alphabet
             -> Int              -- width of each glyph in bytes
             -> [BS.ByteString]  -- input ByteString blocks
             -> [(Char, Glyph)] -- associative list of alphabet letters
loadAlphabet start w bss = zipWith (,) [start .. end] glyphs 
  where
    end = chr $ (ord start) + 25
    glyphs = map (Glyph w) (take 26 $ drop (ord start) bss)

-- Load a FreeBSD bitmap font.
fontLoad :: FilePath   -- path to the font file
         -> (Int, Int) -- width and height of each glyph in bits
         -> IO Font    -- loaded font
fontLoad path (w, h) = do
  content <- BS.readFile path
  let glyphs = splitEvery (w * h `div` 8) content
  let lowerAlphabet = loadAlphabet 'a' (w `div` 8) glyphs 
  let upperAlphabet = loadAlphabet 'A' (w `div` 8) glyphs 
  return $ MP.fromList (upperAlphabet ++ lowerAlphabet)

-- Find a corresponding font glyph for the character.
fontGetGlyph :: Font         -- font
              -> Char        -- requested character
              -> Maybe Glyph -- a glyph after the lookup
fontGetGlyph font char = MP.lookup char font

