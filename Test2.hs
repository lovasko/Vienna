import Brain
import Font
import Data.Maybe (fromJust)
import qualified Data.BitString as BI
import qualified Data.ByteString as BS
import Data.Char
import Data.List (delete)

letterPair :: Font
           -> Char
           -> (BI.BitString, BI.BitString)
letterPair font char = (BI.bitString input, BI.bitString output)
	where
		(Glyph _ input) = fromJust $ lookup lower glyphs
		(Glyph _ output) = fromJust $ lookup upper glyphs
		lower = toLower char
		upper = toUpper char
		glyphs = fontGetAllGlyphs font

test :: Int
     -> Int
		 -> Int
		 -> Char
		 -> IO () 
test2 neuronCount teachLimit askLimit missing = do
	font <- fontLoad "swiss-8x16.fnt" (8, 16)
	emptyBrain <- brainNew 128 neuronCount 

	let alphabet = delete (toLower missing) ['a' .. 'z']
	let letterPairs = map (letterPair font) alphabet
	let (Glyph _ missingLetter) = fromJust $ fontGetGlyph font (toLower missing)

	let brain = foldr (brainTeach teachLimit) emptyBrain letterPairs
	let output = brainAsk brain (BI.bitString missingLetter) askLimit 
	putStrLn $ show $ Glyph 1 (BI.realizeBitStringStrict output)

	return ()

