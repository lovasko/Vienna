import Brain
import Font
import Data.Maybe (fromJust)
import Data.BitString

test1 :: Int -> Int -> IO () 
test1 neuronCount limit = do
	font <- fontLoad "swiss-8x16.fnt" (8, 16)
	let (Letter _ letterA) = fromJust $ fontGetLetter font 'A'
	let (Letter _ letterB) = fromJust $ fontGetLetter font 'B'
	let (Letter _ letterZ) = fromJust $ fontGetLetter font 'Z'
	let (Letter _ letterD) = fromJust $ fontGetLetter font 'D'

	b1 <- brainNew 128 neuronCount
	let b2 = brainTeach b1 (bitString letterA, bitString letterB) limit
	let b3 = brainTeach b2 (bitString letterB, bitString letterZ) limit 

	let output = brainAsk b3 (bitString letterD) limit
	putStrLn $ show $ Letter 1 (realizeBitStringStrict output)
	return ()

