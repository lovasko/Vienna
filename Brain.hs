{-# LANGUAGE TypeSynonymInstances #-}

import qualified Data.ByteString as BS
import qualified Data.Bits as BT
import System.Entropy (getEntropy)
import Control.Monad (replicateM)
import Data.List (partition)
import Data.Int

type Address = BS.ByteString
type Knowledge = BS.ByteString
type Input = BS.ByteString
type Output = BS.ByteString
data Neuron = Neuron Address Knowledge
type Brain = [Neuron]

-- Convert the address ByteString into a string of zeros and ones.
addressString :: Address -> String
addressString address = concatMap byte2string (BS.unpack address)
		where
			byte2string byte = Prelude.concatMap (bool2string . BT.testBit byte) [0..7]
			bool2string True = "1"
			bool2string False = "0"

-- Pretty-printing of the neuron data.
instance Show Neuron where
	show (Neuron address knowledge) = "Neuron " ++ (addressString address)

-- Create a new brain.
newBrain :: Int      -- size of the input in bytes
         -> Int      -- number of neurons in the brain
         -> IO Brain -- output Brain object
newBrain inputSize neuronCount = do
	addresses <- replicateM neuronCount (getEntropy inputSize)
	return [Neuron address emptyKnowledge | address <- addresses]
		where
			emptyKnowledge = BS.replicate (inputSize `div` 8) 0

-- Determine whether a certain neuron gets attracted by the input. This is 
-- defined as the closeness of the Hamming distance between the input vector
-- and the address of the neuron.
isNeuronAttracted :: Input  -- input vector
                  -> Int    -- treshold for the neuron attraction
									-> Neuron -- neuron
									-> Bool   -- decision
isNeuronAttracted input threshold (Neuron address _) = hamming input address < threshold

-- Influence the neuron's knowledge by the output vector.
stimulateNeuron :: Output -- output vector
                -> Neuron -- old neuron
								-> Neuron -- new neuron
stimulateNeuron output (Neuron address knowledge) = Neuron address newKnowledge
	where
		newKnowledge = BS.pack $ map fromIntegral (zipWith change expandOutput expandKnowledge)

		expandOutput :: [Bool]
		expandOutput = concatMap (\x -> map (BT.testBit x) [0..7]) (BS.unpack output)

		expandKnowledge :: [Int8]
		expandKnowledge = map fromIntegral (BS.unpack knowledge)

		change :: Bool -> Int8 -> Int8
		change False n = n-1
		change True n = n+1

-- Teach the brain a new input/output pair.
teach :: Brain           -- old brain
      -> (Input, Output) -- input/output pair
			-> Int             -- threshold for the neuron attraction 
			-> Brain           -- new brain
teach neurons (input, output) threshold = newNeurons
	where
		newNeurons = dontcare ++ (map (stimulateNeuron output) attracted)
		(attracted, dontcare) = partition (isNeuronAttracted input threshold) neurons

-- Compute the Hamming distance between two ByteStrings.
hamming :: BS.ByteString -- first ByteString
        -> BS.ByteString -- second ByteString
        -> Int           -- Hamming distance
hamming as bs = sum $ BS.zipWith byteHamming as bs
	where
		byteHamming a b = BT.popCount $ BT.xor a b

