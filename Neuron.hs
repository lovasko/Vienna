module Neuron 
( Address
, Knowledge
, Neuron(..)
, Input
, Output
, newNeuron
, isNeuronAttracted
, stimulateNeuron
) where

import qualified Data.BitString as BI
import qualified Data.Vector.Unboxed as VU
import Data.Int
import Control.Monad
import System.Random

type Input = BI.BitString
type Output = BI.BitString

type Address = BI.BitString
type Knowledge = VU.Vector Int8
data Neuron = Neuron Address Knowledge

-- Pretty-printing of the neuron data.
instance Show Neuron where
	show (Neuron address knowledge) = "Neuron " ++ (concatMap show (BI.to01List address)) ++ " " ++ (show knowledge)

-- Create a new neuron.
newNeuron :: Int       -- size of the neurons address
          -> IO Neuron -- new neuron
newNeuron size = do
	address <- replicateM size randomIO :: IO [Bool]
	return $ Neuron (BI.fromList address) (VU.replicate size 0) 

-- Determine whether a certain neuron gets attracted by the input. This is 
-- defined as the closeness of the Hamming distance between the input vector
-- and the address of the neuron.
isNeuronAttracted :: Input  -- input vector
                  -> Int    -- treshold for the neuron attraction
									-> Neuron -- neuron
									-> Bool   -- decision
isNeuronAttracted input limit (Neuron address _) = hamming input address < limit 

-- Influence the neuron's knowledge by the output vector.
stimulateNeuron :: Output -- output vector
                -> Neuron -- old neuron
                -> Neuron -- new neuron
stimulateNeuron output (Neuron address knowledge) = Neuron address newKnowledge
	where
		newKnowledge = VU.fromList $ zipWith change (BI.toList output) (VU.toList knowledge)
		change True   127 = 127
		change True     n = n+1
		change False (-127) = -127
		change False    n = n-1

-- Compute the Hamming distance between two bit strings.
hamming :: BI.BitString -- first bit string 
        -> BI.BitString -- second bit string 
        -> Int          -- Hamming distance
hamming a b = sum $ map fromEnum (zipWith (/=) (BI.toList a) (BI.toList b))

