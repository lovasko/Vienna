import Brain
import Font
import qualified Data.ByteString as BS
import qualified Data.BitString as BI
import System.Random
import Control.Monad
import Data.Maybe

noise :: BS.ByteString
      -> Int
      -> Float
			-> IO BI.BitString
noise bs size prob = do
	probs <- replicateM size randomIO :: IO [Float]
	putStrLn $ show probs
	let bits = BI.toList $ BI.bitString bs 
	putStrLn $ show bits
	return $ BI.fromList $ zipWith probNegate probs bits
	where
		probNegate p b
			| p < prob = not b
			| otherwise = b

-- Introduce a random noise to the input a sift it through the brain.
test3 :: Int   -- neuron count
      -> Int   -- teach limit
			-> Int   -- ask limit
      -> Float -- noise probability
      -> IO ()
test3 neuronCount teachLimit askLimit prob = do
	font <- fontLoad "iso-8x16.fnt" (8, 16)
	emptyBrain <- brainNew 128 neuronCount

	-- Build the brain.
	let pairs = map (\(char, (Glyph _ bytes)) -> (BI.bitString bytes, BI.bitString bytes)) (fontGetAllGlyphs font)
	let brain = foldr (brainTeach teachLimit) emptyBrain pairs

	-- Load the glyph and add noise to it.
	input <- getLine
	let (Glyph size bytes) = fromJust $ fontGetGlyph font (head input)
	modified <- noise bytes 128 prob

	-- Display the original glyph.
	putStrLn "Original:"
	putStrLn $ show $ Glyph 1 bytes
	putStrLn "--------"
	
	-- Display the glyph with added noise.
	putStrLn "With noise:"
	putStrLn $ show $ Glyph 1 (BI.realizeBitStringStrict modified)
	putStrLn "--------"

	let answer = brainAsk brain modified askLimit
	putStrLn $ show $ Glyph 1 (BI.realizeBitStringStrict answer)
	return ()

