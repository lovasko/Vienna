import Brain
import Font
import Data.Maybe (fromJust)
import Data.BitString

-- By confusing the brain with two similar inputs, we
-- produce a certain mixture of the output letters.
test1 :: Int
      -> Int
      -> Int
      -> IO () 
test1 neuronCount teachLimit askLimit = do
  font <- fontLoad "swiss-8x16.fnt" (8, 16)
  let (Glyph _ glyph1) = fromJust $ fontGetGlyph font 'i'
  let (Glyph _ glyph2) = fromJust $ fontGetGlyph font 'B'
  let (Glyph _ glyph3) = fromJust $ fontGetGlyph font 't'
  let (Glyph _ glyph4) = fromJust $ fontGetGlyph font 'G'
  let (Glyph _ glyph5) = fromJust $ fontGetGlyph font 'I'

  b1 <- brainNew 128 neuronCount
  let b2 = brainTeach teachLimit (bitString glyph1, bitString glyph2) b1 
  let b3 = brainTeach teachLimit (bitString glyph3, bitString glyph4) b2 

  let output = brainAsk b3 (bitString glyph5) askLimit
  putStrLn $ show $ Glyph 1 (realizeBitStringStrict output)
  return ()

