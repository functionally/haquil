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
-- | Qubits and operations on them, using the indexing conventions of \<<https://arxiv.org/abs/1711.02086/>\>.
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
, pureState
, qubitsOperator
-- * Properties
, wavefunctionOrder
, wavefunctionIndices
, wavefunctionAmplitudes
, rawWavefunction
, operatorOrder
, operatorIndices
, operatorAmplitudes
, rawOperator
-- * Operations
, (^*^)
, (^*)
, (*^)
, (^^*)
, (*^^)
, probabilities
, project
, measure
, wavefunctionProbability
) where


import Control.Arrow (first, second)
import Control.Monad (replicateM)
import Control.Monad.Random.Class (fromList)
import Control.Monad.Random.Lazy (Rand, RandomGen)
import Data.Complex (Complex(..), magnitude)
import Data.Function (on)
import Data.Int.Util (ilog2, isqrt)
import Data.List (elemIndex, groupBy, intersect, intercalate, sortBy)
import Data.Maybe (catMaybes)
import Numeric.LinearAlgebra.Array ((.*))
import Numeric.LinearAlgebra.Array.Util (asScalar, coords, names, order, outers, renameExplicit, reorder)
import Numeric.LinearAlgebra.Tensor (Tensor, Variant(..), listTensor, switch)

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
    zipWith (\k v -> (show k, indexPrefix variant : show v)) [(1+o)..] $ reverse indices


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
  | a == 0    = show b ++ "i"
  | otherwise = "(" ++ show a ++ "+" ++ show b ++ "i)"


-- | Show a state and its amplitude.
type ShowState =  Int                   -- ^ Number of qubits in the state.
               -> ([QState], Amplitude) -- ^ The state and its amplitude.
               -> String                -- ^ The string representation.


-- | Help to compactly show a tensor.
showTensor :: (a -> Tensor Amplitude) -- ^ Function for extracting the tensor of amplitudes.
           -> ShowState               -- ^ Function for showing a state and its amplitude.
           -> a                       -- ^ What to show.
           -> String                  -- ^ The string respresentation.
showTensor toTensor format x =
    let
      x' = canonicalOrder $ toTensor x
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


-- | Transpose a tensor to canonical order.
canonicalOrder :: Tensor Amplitude -> Tensor Amplitude
canonicalOrder x =
  let
    f (v : i) (v' : i') = (v, - read i :: Int) `compare` (v', - read i')
    f _ _ = undefined
  in
    reorder (sortBy f $ names x) x


-- | Amplitudes in a tensor.
tensorAmplitudes :: Tensor Amplitude        -- ^ The tensor.
                 -> [([QState], Amplitude)] -- ^ List of qubit states and their amplitudes, where indices of states are ordered according to 'tensorIndices'.
tensorAmplitudes x =
  let
    n = order x
  in
    zip (replicateM n [minBound..maxBound])
      . V.toList
     $ coords x


-- | A wavefunction for qubits.
newtype Wavefunction = Wavefunction {rawWavefunction' :: Tensor Amplitude}
  deriving (Eq, Floating, Fractional, Num)

instance Show Wavefunction where
  show =
    showTensor rawWavefunction'
      $ \_ (k, v) ->
      showAmplitude v ++ "|" ++ concatMap show k ++ ">"


-- | The 'Tensor' encoding the wavefunction.
rawWavefunction :: Wavefunction -> Tensor Amplitude
rawWavefunction = canonicalOrder . rawWavefunction'


-- | Number of qubits in a wavefunction.
wavefunctionOrder :: Wavefunction -> Int
wavefunctionOrder = order . rawWavefunction'


-- | Qubit indices in a wavefunction.
wavefunctionIndices :: Wavefunction -- ^ The wavefunction.
                    -> [QIndex]     -- ^ List of qubit indices.
wavefunctionIndices = tensorIndices . rawWavefunction'


-- | Amplitudes of states in a qubit wavefunction.
wavefunctionAmplitudes :: Wavefunction            -- ^ The wavefunction.
                       -> [([QState], Amplitude)] -- ^ List of qubit states and their amplitudes, where indices of states are ordered according to 'wavefunctionIndices'.
wavefunctionAmplitudes = tensorAmplitudes . rawWavefunction'


-- | Construct a qubit from the amplitudes of its states.
--
-- The squares of the norms of the amplitudes must sum to one.
qubit :: QIndex                 -- ^ The index of the qubit in the wavefunction.
      -> (Amplitude, Amplitude) -- ^ The amplitude of the 0 and 1 states, respectively.
      -> Wavefunction           -- ^ The wavefunction for the qubit.
qubit index (a0, a1) =
  Wavefunction
    . renameExplicit [("1", indexPrefix Contra : show index)]
    $ listTensor [2] [a0, a1]


-- | Construct a qubit with a pure state.
pureQubit :: QIndex       -- ^ Which qubit.
          -> QState       -- ^ The state of the qubit.
          -> Wavefunction -- ^ The wavefunction.
pureQubit index QState0 = qubit index (1, 0)
pureQubit index QState1 = qubit index (0, 1)


-- | Construct a wavefunction for the amplitudes of its qubit states.
--
-- Amplitudes ordered so that the 0 state appears before the 1 state and the lower qubit indices cycle faster than then higher qubit indices. For example, a two-qubit state has its amplitudes ordered |00>, |01>, |10>, |11>. This ordering can be generated as follows, where qubits are orderd from higher indices to lower ones:
--
-- >>> sequence $ replicate 3 [minBound..maxBound] :: [[QState]]
-- [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]
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
groundState = pureState . flip replicate QState0


-- | Constructa a pure state.
pureState :: [QState]     -- ^ The state of each qubit, ordered from higher index to lower index.
          -> Wavefunction -- ^ The wavefunction.
pureState =
  Wavefunction
    . outers
    . (rawWavefunction' <$>)
    . (uncurry pureQubit <$>)
    . zip [0..]
    . reverse


-- | An operator on wavefunctions.
newtype Operator = Operator {rawOperator' :: Tensor (Complex Double)}
  deriving (Eq, Floating, Fractional, Num)

instance Show Operator where
  show =
    showTensor rawOperator'
      $ \n (k, v) ->
      let
        (kr, kc) = splitAt (n `div` 2) $ concatMap show k
      in
        showAmplitude v ++ "|" ++ kr ++ "><" ++ kc ++ "|"


-- | The 'Tensor' encoding the operator.
rawOperator :: Operator -> Tensor Amplitude
rawOperator = canonicalOrder . rawOperator'


-- | Number of qubits for an operator.
operatorOrder :: Operator -> Int
operatorOrder = isqrt . order . rawOperator'


-- | Qubit indices in an operator.
operatorIndices :: Operator -- ^ The operator.
                -> [QIndex] -- ^ List of qubit indices.
operatorIndices = tensorIndices . rawOperator'


-- | Amplitudes of state transitions in a qubit operator.
operatorAmplitudes :: Operator                            -- ^ The wavefunction.
                   -> [(([QState], [QState]), Amplitude)] -- ^ List of qubit state transitions and their amplitudes, in row-major order with the states ordered according to 'operatorIndices'.
operatorAmplitudes (Operator x) =
  let
    x' = tensorAmplitudes x
    n = length . fst $ head x'
  in
    first (splitAt $ n `div` 2) <$> x'


-- | Construct an operator on qubit wavefunctions.
--
-- Amplitudes in row-major order where amplitudes are ordered so that the 0 state appears before the 1 state and the lower qubit indices cycle faster than then higher qubit indices. For example, a three-qubit operator has its amplitudes ordered \<00|00>, \<00|01>, \<00|10>, \<00|11>, \<01|00>, \<01|01>, \<01|10>, \<01|11>, \<10|00>, \<10|01>, \<10|10>, \<10|11>, \<11|00>, \<11|01>, \<11|10>, \<11|11>, where states in the bras and kets are correspond to the order of the first argument to `qubitsOperator`. This ordering can be generated as follows:
--
-- >>> fmap (splitAt 2) . sequence $ replicate (2 * 2) [minBound..maxBound] :: [([QState], [QState])] 
-- [([0,0],[0,0]),([0,0],[0,1]),([0,0],[1,0]),([0,0],[1,1]),([0,1],[0,0]),([0,1],[0,1]),([0,1],[1,0]),([0,1],[1,1]),([1,0],[0,0]),([1,0],[0,1]),([1,0],[1,0]),([1,0],[1,1]),([1,1],[0,0]),([1,1],[0,1]),([1,1],[1,0]),([1,1],[1,1])]
--
-- The operator must be unitary.
qubitsOperator :: [QIndex]    -- ^ The qubit indices for which the operator applies, in descending order according to \<<https://arxiv.org/pdf/1711.02086/>\>.
               -> [Amplitude] -- ^ The amplitudes of the operator matrix, in row-major order with the states ordered from higher indices to lower ones.
               -> Operator    -- ^ The wavefunction operator.
qubitsOperator indices =
  let
    n = length indices
  in
    Operator
      . renameExplicit (operatorLabels $ reverse indices)
      . listTensor (replicate n 2 ++ replicate n (-2))


-- | Multiply two wavefunction or operator tensors.
mult :: Tensor Amplitude -> Tensor Amplitude -> Tensor Amplitude
mult x y =
  let
    lft  = filter ((== indexPrefix Co    ) . head) $ names x
    rght = filter ((== indexPrefix Contra) . head) $ names y
    cmmn = fmap tail lft `intersect` fmap tail rght
    x' = renameExplicit [(i, '@' : tail i) | i <- filter ((`elem` cmmn) . tail) lft ] x
    y' = renameExplicit [(i, '@' : tail i) | i <- filter ((`elem` cmmn) . tail) rght] y
  in
    x' * y' 


-- | Apply two operators in sequence.
(^*^) :: Operator -> Operator -> Operator
Operator x ^*^ Operator y = Operator $ x `mult` y
infixr 7 ^*^


-- | Apply an operator to a wavefunction.
(^*) :: Operator -> Wavefunction -> Wavefunction
Operator x ^* Wavefunction y = Wavefunction $ x `mult` y
infixr 6 ^*


-- | Apply an operator to a wavefunction.
(*^) :: Wavefunction -> Operator -> Wavefunction
(*^) = flip (^*)
infixl 6 *^


-- | Apply a sequence of operators to a wavefunction.
(^^*) :: Foldable t => t Operator -> Wavefunction -> Wavefunction
(^^*) = flip . foldl $ flip (^*)
infixr 6 ^^*


-- | Apply a sequence of operators to a wavefunction.
(*^^) :: Foldable t => Wavefunction -> t Operator -> Wavefunction
(*^^) = flip (^^*)
infixl 6 *^^


-- | Probabilities of a selection of qubits.
probabilities :: [QIndex]                       -- ^ Which qubits.
              -> Wavefunction                   -- ^ The wavefunciton.
              -> [([(QIndex, QState)], Double)] -- ^ The probabilities for the combinations of qubit states.
probabilities indices x =
  let
    indices' = wavefunctionIndices x
    positions = catMaybes $ (`elemIndex` indices') <$> indices
  in
    filter ((/= 0) . snd)
    . fmap (\kvs -> (fst $ head kvs, sum $ snd <$> kvs))
    . groupBy ((==) `on` fst)
    $ sortBy (compare `on` fst)
      [
        (
          fmap snd . filter ((`elem` positions) . fst) . zip [0..] $ zip indices' states
        , magnitude amplitude ^ (2::Int)
        )
      |
        (states, amplitude) <- wavefunctionAmplitudes x
      ]


-- | Project a wavefunction onto a particular state.
project :: [(QIndex, QState)] -- ^ The qubits for the state.
        -> Wavefunction       -- ^ The wavefunction.
        -> Wavefunction       -- ^ The projected wavefunction.
project states x =
  let
    y = canonicalOrder $ rawWavefunction' x
    p =
      canonicalOrder
        . outers
        $ fmap rawWavefunction'
        [
          case i `lookup` states of
            Nothing -> qubit i (1, 1)
            Just s  -> pureQubit i s
        |
          i <- [0..(order y - 1)]
        ]
    z = p .* y
  in
    Wavefunction $ z / sqrt (z * switch z)


-- | Measure qubits in a wavefunction.
measure :: RandomGen g
        => [QIndex]                                  -- ^ Which qubits to measure.
        -> Wavefunction                              -- ^ The wavefunction.
        -> Rand g ([(QIndex, QState)], Wavefunction) -- ^ Action for the resulting measurement and wavefunction.
measure indices x =
  do
    let
      candidates = probabilities indices x
    collapse <- fromList $ second toRational <$> candidates
    return (collapse, project collapse x)


-- | The total probability for the wave function, which should be 1.
wavefunctionProbability :: Wavefunction
                        -> Amplitude
wavefunctionProbability (Wavefunction x) = asScalar $ x * switch x
