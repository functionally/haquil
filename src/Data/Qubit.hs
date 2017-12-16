-----------------------------------------------------------------------------
--
-- Module      :  $Header$
-- Copyright   :  (c) 2017 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Qubits and operations on them.
--
-----------------------------------------------------------------------------


{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Data.Qubit (
-- * Types
  QIndex
, QState(..)
, Amplitude
, Wavefunction
, Operator
-- * Construction
, qubit
, qubits
, groundState
, qubitsOperator
-- * Properties
, wavefunctionOrder
, wavefunctionIndices
, wavefunctionAmplitudes
, operatorOrder
, operatorIndices
, operatorAmplitudes
-- * Operations
, (.*.)
, (.*)
, (..*)
) where


import Control.Arrow (first)
import Data.Complex (Complex(..))
import Data.Int.Util (ilog2, isqrt)
import Data.List (intersect, intercalate)
import Numeric.LinearAlgebra.Array.Util (coords, names, order, renameExplicit)
import Numeric.LinearAlgebra.Tensor (Tensor, Variant(..), listTensor)

import qualified Data.Vector.Storable as V (toList)


-- | States of a quibit.
data QState =
    QState0
  | QState1
    deriving (Bounded, Enum, Eq, Ord)

instance Read QState where
  readsPrec = (fmap (first toEnum) .) . readsPrec

instance Show QState where
  show = show . fromEnum


-- | Index for qubits in a wavefunction.
type QIndex = Int


-- | Prefixes for 'Tensor' indices.
indexPrefix :: Variant -> Char
indexPrefix Contra = 'm'
indexPrefix Co     = 'n'


-- | 'Tensor' index for a collection of qubits.
indexLabels :: Variant            -- ^ Whether contravariant or covariant.
            -> [QIndex]           -- ^ Indices of the qubits.
            -> [(String, String)] -- ^ Pairs of default `Tensor` indices and qubit indices.
indexLabels variant indices =
  let
    n = length indices
    o =
      case variant of
        Contra -> 0
        Co     -> n
  in
    zipWith (\k v -> (show k, indexPrefix variant : show v)) [(1+o)..] indices


-- | 'Tensor' index for a wavefunction.
wavefunctionLabels :: Int                -- ^ The number of qubits.
                   -> [(String, String)] -- ^ Pairs of default 'Tensor' indices and qubit indices.
wavefunctionLabels n = indexLabels Contra [0..(n-1)]


-- | 'Tensor' index for an operator on qubits.
operatorLabels :: [QIndex]           -- ^ Indices of the qubits.
               -> [(String, String)] -- ^ Pairs of default `Tensor` indices and qubit indices.
operatorLabels indices = indexLabels Contra indices ++ indexLabels Co indices


-- | Amplitude of a state  in a wavefunction.
type Amplitude = Complex Double


-- | Compactly show an amplitude.
showAmplitude :: Amplitude -> String
showAmplitude (a :+ b)
  | b == 0    = show a
  | a == 0    = show b ++ "j"
  | otherwise = "(" ++ show a ++ "+" ++ show b ++ "j)"


-- | Show a state and its amplitude.
type ShowState =  Int                   -- ^ Number of qubits in the state.
               -> ([QState], Amplitude) -- ^ The state and its amplitude.
               -> String                -- ^ The string representation.


-- | Help to ompactly show a tensor.
showTensor :: (a -> Tensor Amplitude) -- ^ Function for extracting the tensor of amplitudes.
           -> ShowState               -- ^ Function for showing a state and its amplitude.
           -> a                       -- ^ What to show.
           -> String                  -- ^ The string respresentation.
showTensor toTensor format x =
    let
      x' = toTensor x
      n = order x'
    in
      (++ (show $ tensorIndices x'))
      . (++ " @ ")
      . intercalate " + "
        . fmap (format n)
        . filter ((/= 0) . snd)
        $ tensorAmplitudes x'


-- | Indices in a tensor.
tensorIndices :: Tensor Amplitude -- ^ The tensor.
              -> [QIndex]         -- ^ The qubit indices.
tensorIndices =
  fmap (read . tail)
    . filter ((== indexPrefix Contra) . head)
    . names


-- | Amplitudes in a tensor.
tensorAmplitudes :: Tensor Amplitude  -- ^ The tensor.
           -> [([QState], Amplitude)] -- ^ List of qubit states and their amplitudes.
tensorAmplitudes x =
  let
    n = order x
  in
    zip (fmap reverse . sequence $ replicate n [minBound..maxBound])
      . V.toList
      $ coords x


-- | A wavefunction for qubits.
newtype Wavefunction = Wavefunction {wavefunction :: Tensor Amplitude}
  deriving (Eq, Floating, Fractional, Num)

instance Show Wavefunction where
  show =
    showTensor wavefunction
      $ \_ (k, v) ->
      showAmplitude v ++ "|" ++ concatMap show k ++ ">"


-- | Number of qubits in a wavefunction.
wavefunctionOrder :: Wavefunction -> Int
wavefunctionOrder = order . wavefunction


-- | Qubit indices in a wavefunction.
wavefunctionIndices :: Wavefunction -- ^ The wavefunction.
                    -> [QIndex]     -- ^ List of qubit indices.
wavefunctionIndices = tensorIndices . wavefunction


-- | Amplitudes of states in a qubit wavefunction.
wavefunctionAmplitudes :: Wavefunction            -- ^ The wavefunction.
                       -> [([QState], Amplitude)] -- ^ List of qubit states and their amplitudes.
wavefunctionAmplitudes = tensorAmplitudes . wavefunction


-- | Construct a qubit from the amplitudes of its states.
--
-- The squares of the norms of the amplitudes must sum to one.
qubit :: Amplitude    -- ^ The amplitude of the 0 state.
      -> Amplitude    -- ^ The amplitude of the 1 state.
      -> Wavefunction -- ^ The wavefunction for the qubit.
qubit = (qubits .) . (. return) . (:)


-- | Construct a wavefunction for the amplitudes of its qubit states.
--
-- Amplitudes ordered so that the 0 state appears before the 1 state and the lower qubit indices cycle faster than then higher qubit indices. For example, a two-qubit state has its amplitudes ordered |00>, |10>, |01>, |11>. This ordering can be generated as follows:
--
-- >>> fmap reverse . sequence $ replicate 3 [minBound..maxBound] :: [[QState]]
-- [[0,0,0],[1,0,0],[0,1,0],[1,1,0],[0,0,1],[1,0,1],[0,1,1],[1,1,1]]
--
-- The squares of the norms of the amplitudes must sum to one.
qubits :: [Amplitude]  -- ^ The amplitudes.
       -> Wavefunction -- ^ The wavefunction.
qubits amplitudes =
  let
    n = ilog2 $ length amplitudes
  in
    Wavefunction
      . renameExplicit (wavefunctionLabels n)
      $ listTensor (replicate n 2) amplitudes


-- | Construct the ground state where each qubit is in state 0.
groundState :: Int          -- ^ Number of qubits.
            -> Wavefunction -- ^ The ground state wavefunction.
groundState = qubits . (1 :) . (`replicate` 0) . (+ (-1)) . (2^)


-- | An operator on wavefunctions.
newtype Operator = Operator {operator :: Tensor (Complex Double)}
  deriving (Eq, Floating, Fractional, Num)

instance Show Operator where
  show =
    showTensor operator
      $ \n (k, v) ->
      let
        (k0, k1) = splitAt (n `div` 2) $ concatMap show k
      in
        showAmplitude v ++ "|" ++ k0 ++ "><" ++ k1 ++ "|"


-- | Number of qubits for an operator.
operatorOrder :: Operator -> Int
operatorOrder = isqrt . order . operator


-- | Qubit indices in an operator.
operatorIndices :: Operator -- ^ The operator.
                -> [QIndex] -- ^ List of qubit indices.
operatorIndices = tensorIndices . operator


-- | Amplitudes of state transitions in a qubit operator.
operatorAmplitudes :: Operator                            -- ^ The wavefunction.
                   -> [(([QState], [QState]), Amplitude)] -- ^ List of qubit state transitions and their amplitudes.
operatorAmplitudes (Operator x) =
  let
    x' = tensorAmplitudes x
    n = length . fst $ head x'
  in
    first (splitAt $ n `div` 2) <$> x'


-- | Construct an operator on qubit wavefunctions.
--
-- Amplitudes in row-major order where qubit states are ordered so that the 0 state appears before the 1 state and the lower qubit indices cycle faster than then higher qubit indices. For example, a three-qubit operator has its amplitudes ordered \<00|00>, \<00|10>, \<00|01>, \<00|11>, \<10|00>, \<10|10>, \<10|01>, \<10|11>, \<01|00>, \<01|10>, \<01|01>, \<01|11>, \<11|00>, \<11|10>, \<11|01>, \<11|11>. This ordering can be generated as follows:
--
-- >>> fmap (swap . splitAt 2 .reverse) . sequence $ replicate (2 * 2) [minBound..maxBound] :: [([QState], [QState])] 
-- [[0,0,0],[1,0,0],[0,1,0],[1,1,0],[0,0,1],[1,0,1],[0,1,1],[1,1,1]],1],[0,0]),([0,1],[1,0]),([0,1],[0,1]),([0,1],[1,1]),([1,1],[0,0]),([1,1],[1,0]),([1,1],[0,1]),([1,1],[1,1])]
--
-- The operator must be unitary.
qubitsOperator :: [QIndex]    -- ^ The qubit indices for which the operator applies.
               -> [Amplitude] -- ^ The amplitudes of the operator matrix.
               -> Operator    -- ^ The wavefunction operator.
qubitsOperator indices =
  let
    n = length indices
  in
    Operator
      . renameExplicit (operatorLabels indices)
      . listTensor (replicate n 2 ++ replicate n (-2))


-- | Multiply to wavefunction tensors.
mult :: Tensor Amplitude -> Tensor Amplitude -> Tensor Amplitude
mult x y =
  let
    lft  = filter ((== 'n') . head) $ names x
    rght = filter ((== 'm') . head) $ names y
    cmmn = fmap tail lft `intersect` fmap tail rght
    x' = renameExplicit [(i, 'p' : tail i) | i <- filter ((`elem` cmmn) . tail) lft ] x
    y' = renameExplicit [(i, 'p' : tail i) | i <- filter ((`elem` cmmn) . tail) rght] y
  in
    x' * y' 


-- | Apply two operators in sequence.
(.*.) :: Operator -> Operator -> Operator
Operator x .*. Operator y = Operator $ x `mult` y


-- | Apply an operator to a wavefunction.
(.*) :: Operator -> Wavefunction -> Wavefunction
Operator x .* Wavefunction y = Wavefunction $ x `mult` y


-- | Apply a sequence of operators to a wavefunction.
(..*) :: Foldable t => t Operator -> Wavefunction -> Wavefunction
(..*) = flip . foldl $ flip (.*)
