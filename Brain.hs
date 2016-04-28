module Brain
( Brain
, brainNew
, brainTeach
, brainAsk
) where

import qualified Data.ByteString as BS
import qualified Data.Bits as BT
import System.Random (randomIO)
import Control.Monad (replicateM)
import Data.List (partition)
import Data.Int
import qualified Data.Vector.Unboxed as VU
import qualified Data.BitString as BI

import Neuron

type Brain = [Neuron]

-- Create a new brain.
brainNew :: Int      -- size of the input in bytes
         -> Int      -- number of neurons in the brain
         -> IO Brain -- output Brain object
brainNew inputSize neuronCount = replicateM neuronCount (newNeuron inputSize)

-- Teach the brain a new input/output pair.
brainTeach :: Brain           -- old brain
           -> (Input, Output) -- input/output pair
           -> Int             -- threshold for the neuron attraction 
           -> Brain           -- new brain
brainTeach neurons (input, output) threshold = newNeurons
  where
    newNeurons = dontcare ++ (map (stimulateNeuron output) attracted)
    (attracted, dontcare) = partition (isNeuronAttracted input threshold) neurons

-- Ask the brain about a certain input.
brainAsk :: Brain  -- brain
         -> Input  -- input
         -> Int    -- threshold for the neuron attraction
         -> Output -- output
brainAsk neurons input limit = BI.fromList $ map (>0) coefficients 
  where
    coefficients = foldr step initial knowledges
    step x y = zipWith (+) y (map fromIntegral (VU.toList x))
    initial = replicate (knowledgeLength $ head neurons) 0 :: [Integer]
    knowledgeLength (Neuron address knowledge) = VU.length knowledge
    knowledges = [knowledge | (Neuron _ knowledge) <- attracted]
    (attracted, dontcare) = partition (isNeuronAttracted input limit) neurons

