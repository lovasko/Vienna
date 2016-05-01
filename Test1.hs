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
  let (Letter _ letter1) = fromJust $ fontGetLetter font 'i'
  let (Letter _ letter2) = fromJust $ fontGetLetter font 'B'
  let (Letter _ letter3) = fromJust $ fontGetLetter font 't'
  let (Letter _ letter4) = fromJust $ fontGetLetter font 'G'
  let (Letter _ letter5) = fromJust $ fontGetLetter font 'I'

  b1 <- brainNew 128 neuronCount
  let b2 = brainTeach b1 (bitString letter1, bitString letter2) teachLimit
  let b3 = brainTeach b2 (bitString letter3, bitString letter4) teachLimit 

  let output = brainAsk b3 (bitString letter5) askLimit
  putStrLn $ show $ Letter 1 (realizeBitStringStrict output)
  return ()

