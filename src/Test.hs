-----------------------------------------------------------------------------
--
-- Module      :  $Header$
-- Copyright   :  (c) 2017-18 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Testing qbits and operations on them.
--
-- The test data is derived from pyQuil \<<http://pyquil.readthedocs.io>\ and recorded in \<<https://bitbucket.org/functionally/quil/src/99a2efb54fd2c7ac99a9bde3091da130bdadbf6b/test-data.ipynb>\>.
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module Main (
  main
) where


import Control.Monad (replicateM)
import Control.Monad.Random.Lazy (evalRandIO)
import Data.BitVector (BV)
import Data.Bits (bit, complementBit, setBit, testBit, xor)
import Data.Complex (Complex(..), cis, magnitude)
import Data.Int.Util (ilog2)
import Data.Qubit (QState(..), (^*), groundState, pureQubit, pureState, qubit, qubits, rawWavefunction, wavefunctionAmplitudes, wavefunctionIndices, wavefunctionOrder)
import Language.Quil.Execute (compileExpression, executeInstruction, runProgramWithStdRandom)
import Language.Quil.Types (Address, BitData(..), Expression(..), Instruction(..), Machine(..), boolFromBitVector, complexFromBitVector, doubleFromBitVector, finiteBitsFromBitVector, integerFromBitVector, machine, toBitVector)
import Numeric.LinearAlgebra.Array ((.*))
import Numeric.LinearAlgebra.Array.Util (coords, scalar)
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.Property (Property)
import System.Exit (exitFailure, exitSuccess)

import qualified Data.Qubit.Gate as G
import qualified Data.Vector as V (empty, fromList)
import qualified Data.Vector.Storable as V (toList)


-- Helper functions.

clearBit x k =
  if testBit x k
    then x `xor` bit k
    else x

few = (+ 1) . (`mod` 10)

i = scalar $ 0 :+ 1

cos' = scalar . (:+ 0) . cos
sin' = scalar . (:+ 0) . sin
cis' = scalar . cis

applyGate = (rawWavefunction .) . (. (qubits . V.toList . coords)) . (^*)

epsilon = 1e-5

xs ~~ ys = V.toList (coords xs) ~~~ V.toList (coords ys)
infix 4 ~~

(~~~) = (and .) . zipWith (\x y -> magnitude (x - y) <= epsilon)


-- Wavefunction tests.

prop_qubit n a0 a1 =
  let
    q = qubit n (a0, a1)
  in
       wavefunctionOrder q == 1
    && wavefunctionIndices q == [n]
    && wavefunctionAmplitudes q == [([QState0], a0), ([QState1], a1)]

prop_pure_qubit n b =
  let
    z = if b then QState1 else QState0
    q = pureQubit n z
  in
       wavefunctionOrder q == 1
    && wavefunctionIndices q == [n]
    && fmap fst (wavefunctionAmplitudes q) == [[QState0], [QState1]]
    && fmap snd (wavefunctionAmplitudes q) ~~~ (if b then [0, 1] else [1, 0])

prop_qubits as =
  let
    n = ilog2 $ length as
    as' = take (2^n) as
    q = qubits as'
  in
       length as < 2
    || n > 15
    || wavefunctionOrder q == n
    && wavefunctionIndices q == reverse [0..(n-1)]
    && fmap fst (wavefunctionAmplitudes q) == replicateM n [minBound..maxBound]
    && fmap snd (wavefunctionAmplitudes q) ~~~ as'

prop_ground n =
  let
    q = groundState n
  in
       n < 2
    || n > 15
    || wavefunctionOrder q == n
    && wavefunctionIndices q == [0..(n-1)]
    && fmap fst (wavefunctionAmplitudes q) == replicateM n [minBound..maxBound]
    && fmap snd (wavefunctionAmplitudes q) ~~~ (1 : replicate (2^n - 1) 0)

prop_pure_state bs =
  let
    n = length bs
    z = [if b then QState1 else QState0 | b <- bs]
    q = pureState z
  in
       n < 2
    || n > 15
    || wavefunctionOrder q == n
    && wavefunctionIndices q == [0..(n-1)]
    && fmap fst (wavefunctionAmplitudes q) == replicateM n [minBound..maxBound]
    && fmap snd (wavefunctionAmplitudes q) ~~~ [if z' == reverse z then 1 else 0 | z' <- replicateM n [minBound..maxBound]]
   

-- Tests for one-qubit gates.

b0 = rawWavefunction $ pureState [QState0]
b1 = rawWavefunction $ pureState [QState1]

prop_gate_i =
  let
    g = applyGate $ G.i 0
  in
       g b0 ~~ b0
    && g b1 ~~ b1

prop_gate_x =
  let
    g = applyGate $ G.x 0
  in
       g b0 ~~ b1
    && g b1 ~~ b0

prop_gate_y =
  let
    g = applyGate $ G.y 0
  in
       g b0 ~~ i .* b1
    && g b1 ~~ - i .* b0

prop_gate_z =
  let
    g = applyGate $ G.z 0
  in
       g b0 ~~ b0
    && g b1 ~~ - b1

prop_gate_h =
  let
    g = applyGate $ G.h 0
  in
       g b0 ~~ (b0 + b1) / sqrt 2
    && g b1 ~~ (b0 - b1) / sqrt 2

prop_gate_phase theta =
  let
    g = applyGate $ G.phase theta 0
  in
       g b0 ~~ b0
    && g b1 ~~ cis' theta .* b1

prop_gate_s =
  let
    g = applyGate $ G.s 0
  in
       g b0 ~~ b0
    && g b1 ~~ i .* b1

prop_gate_t =
  let
    g = applyGate $ G.t 0
  in
       g b0 ~~ b0
    && g b1 ~~ scalar (1 :+ 1) .* b1 / sqrt 2

prop_gate_rx theta =
  let
    g = applyGate $ G.rx theta 0
    theta' = theta / 2
  in
       g b0 ~~   cos' theta'      .* b0 - sin' theta' .* i .* b1
    && g b1 ~~ - sin' theta' .* i .* b0 + cos' theta'      .* b1

prop_gate_ry theta =
  let
    g = applyGate $ G.ry theta 0
    theta' = theta / 2
  in
       g b0 ~~   cos' theta' .* b0 + sin' theta' .* b1
    && g b1 ~~ - sin' theta' .* b0 + cos' theta' .* b1

prop_gate_rz theta =
  let
    g = applyGate $ G.rz theta 0
    theta' = theta / 2
  in
       g b0 ~~ cis' (- theta') .* b0
    && g b1 ~~ cis'    theta'  .* b1


-- Tests for two-cubit gates.

b00 = rawWavefunction $ pureState [QState0, QState0]
b01 = rawWavefunction $ pureState [QState0, QState1]
b10 = rawWavefunction $ pureState [QState1, QState0]
b11 = rawWavefunction $ pureState [QState1, QState1]

prop_gate_cphase00 theta =
  let
    g = applyGate $ G.cphase00 theta 1 0
  in
       g b00 ~~ cis' theta .* b00
    && g b01 ~~ b01
    && g b10 ~~ b10
    && g b11 ~~ b11

prop_gate_cphase01 theta =
  let
    g = applyGate $ G.cphase01 theta 1 0
  in
       g b00 ~~ b00
    && g b01 ~~ cis' theta .* b01
    && g b10 ~~ b10
    && g b11 ~~ b11

prop_gate_cphase10 theta =
  let
    g = applyGate $ G.cphase10 theta 1 0
  in
       g b00 ~~ b00
    && g b01 ~~ b01
    && g b10 ~~ cis' theta .* b10
    && g b11 ~~ b11

prop_gate_cphase theta =
  let
    g = applyGate $ G.cphase theta 1 0
  in
       g b00 ~~ b00
    && g b01 ~~ b01
    && g b10 ~~ b10
    && g b11 ~~ cis' theta .* b11

prop_gate_cnot =
  let
    g = applyGate $ G.cnot 1 0
  in
       g b00 ~~ b00
    && g b01 ~~ b01
    && g b10 ~~ b11
    && g b11 ~~ b10

prop_gate_pswap theta =
  let
    g = applyGate $ G.pswap theta 1 0
  in
       g b00 ~~ b00
    && g b01 ~~ cis' theta .* b10
    && g b10 ~~ cis' theta .* b01
    && g b11 ~~ b11

prop_gate_swap =
  let
    g = applyGate $ G.swap 1 0
  in
       g b00 ~~ b00
    && g b01 ~~ b10
    && g b10 ~~ b01
    && g b11 ~~ b11

prop_gate_iswap =
  let
    g = applyGate $ G.iswap 1 0
  in
       g b00 ~~ b00
    && g b01 ~~ i .* b10
    && g b10 ~~ i .* b01
    && g b11 ~~ b11

prop_gate_cz =
  let
    g = applyGate $ G.cz 1 0
  in
       g b00 ~~ b00
    && g b01 ~~ b01
    && g b10 ~~ b10
    && g b11 ~~ - b11


-- Tests for three-cubit gates.

b000 = rawWavefunction $ pureState [QState0, QState0, QState0]
b001 = rawWavefunction $ pureState [QState0, QState0, QState1]
b010 = rawWavefunction $ pureState [QState0, QState1, QState0]
b011 = rawWavefunction $ pureState [QState0, QState1, QState1]
b100 = rawWavefunction $ pureState [QState1, QState0, QState0]
b101 = rawWavefunction $ pureState [QState1, QState0, QState1]
b110 = rawWavefunction $ pureState [QState1, QState1, QState0]
b111 = rawWavefunction $ pureState [QState1, QState1, QState1]

prop_gate_ccnot =
  let
    g = applyGate $ G.ccnot 2 1 0
  in
       g b000 ~~ b000
    && g b001 ~~ b001
    && g b010 ~~ b010
    && g b011 ~~ b011
    && g b100 ~~ b100
    && g b101 ~~ b101
    && g b110 ~~ b111
    && g b111 ~~ b110

prop_gate_cswap =
  let
    g = applyGate $ G.cswap 2 1 0
  in
       g b000 ~~ b000
    && g b001 ~~ b001
    && g b010 ~~ b010
    && g b011 ~~ b011
    && g b100 ~~ b100
    && g b101 ~~ b110
    && g b110 ~~ b101
    && g b111 ~~ b111

-- Bit data.

prop_bitdata_bool x =
  x == boolFromBitVector 0 (toBitVector $ BoolBit x)

prop_bitdata_int8 x =
  x == finiteBitsFromBitVector 0 (toBitVector $ IntBits8 x)

prop_bitdata_int16 x =
  x == finiteBitsFromBitVector 0 (toBitVector $ IntBits16 x)

prop_bitdata_int32 x =
  x == finiteBitsFromBitVector 0 (toBitVector $ IntBits32 x)

prop_bitdata_int64 x =
  x == finiteBitsFromBitVector 0 (toBitVector $ IntBits64 x)

prop_bitdata_word8 x =
  x == finiteBitsFromBitVector 0 (toBitVector $ WordBits8 x)

prop_bitdata_word16 x =
  x == finiteBitsFromBitVector 0 (toBitVector $ WordBits16 x)

prop_bitdata_word32 x =
  x == finiteBitsFromBitVector 0 (toBitVector $ WordBits32 x)

prop_bitdata_word64 x =
  x == finiteBitsFromBitVector 0 (toBitVector $ WordBits64 x)

prop_bitdata_integer x =
  x < 0 || x == integerFromBitVector 0 n (toBitVector $ IntegerBits n x)
    where
      n = if x == 0 then 1 else fromIntegral (ilog2 x + 1)

prop_bitdata_double x =
  x == doubleFromBitVector 0 (toBitVector $ DoubleBits x)

prop_bitdata_complex x =
  x == complexFromBitVector 0 (toBitVector $ ComplexBits x)


-- Expressions.

prop_expression_power x y =
  x ** y == compileExpression V.empty (Power (Number x) (Number y)) V.empty

prop_expression_times x y =
  x * y == compileExpression V.empty (Times (Number x) (Number y)) V.empty

prop_expression_divide x y =
  y == 0 || x / y == compileExpression V.empty (Divide (Number x) (Number y)) V.empty

prop_expression_plus x y =
  x + y == compileExpression V.empty (Plus (Number x) (Number y)) V.empty

prop_expression_minus x y =
  x - y == compileExpression V.empty (Minus (Number x) (Number y)) V.empty

prop_expression_negate x =
  - x == compileExpression V.empty (Negate $ Number x) V.empty

prop_expression_sin x =
  sin x == compileExpression V.empty (Sin $ Number x) V.empty

prop_expression_cos x =
  cos x == compileExpression V.empty (Cos $ Number x) V.empty

prop_expression_sqrt x =
  sqrt x == compileExpression V.empty (Sqrt $ Number x) V.empty

prop_expression_exp x =
  exp x == compileExpression V.empty (Exp $ Number x) V.empty

prop_expression_cis x =
  cis x == compileExpression V.empty (Cis .Number $ x :+ 0) V.empty

prop_expression_number x =
  x == compileExpression V.empty (Number x) V.empty

prop_expression_variable x y z w =
  y == compileExpression (V.fromList [x, z]) (Variable x) (V.fromList [y, w])


-- Classical bits.

classicalSingle :: (Address -> Instruction) -> (BV -> Int -> BV) -> Integer -> Int -> Property
classicalSingle g f x k =
  monadicIO
    $ do
      let
        n = if x <= 0 then 1 else fromIntegral (ilog2 x + 1)
        k' = k `mod` n
        m = machine 1 [IntegerBits n x]
      m' <- run . evalRandIO $ executeInstruction (g k') m
      assert $ x < 0 || f (cstate m) k' == cstate m'

classicalDouble :: (Address -> Address -> Instruction) -> (BV -> Int -> Int -> BV) -> Integer -> Int -> Int -> Property
classicalDouble g f x k l =
  monadicIO
    $ do
      let
        n = if x <= 0 then 1 else fromIntegral (ilog2 x + 1)
        k' = k `mod` n
        l' = l `mod` n
        m = machine 1 [IntegerBits n x]
      m' <- run . evalRandIO $ executeInstruction (g k' l') m
      assert $ x < 0 || f (cstate m) k' l' == cstate m'

prop_classical_false =
  classicalSingle FALSE clearBit

prop_classical_true =
  classicalSingle TRUE setBit

prop_classical_not =
  classicalSingle NOT complementBit

prop_classical_and =
  classicalDouble AND
    $ \bv k l -> 
      if testBit bv k && testBit bv l
        then bv `setBit` l
        else bv `clearBit` l


prop_classical_or =
  classicalDouble OR
    $ \bv k l -> 
      if testBit bv k || testBit bv l
        then bv `setBit` l
        else bv `clearBit` l

prop_classical_move =
  classicalDouble MOVE
    $ \bv k l -> 
      if testBit bv k
        then bv `setBit` l
        else bv `clearBit` l

prop_classical_exchange =
  classicalDouble EXCHANGE
    $ \bv k l -> 
      case (testBit bv k, testBit bv l) of
        (False, True ) -> bv `setBit`   k `clearBit` l
        (True , False) -> bv `clearBit` k `setBit`   l
        _              -> bv


-- Quantum bits.

prop_quantum_reset n =
  monadicIO
    $ do
      let n' = few n
      Machine{..} <- run $ runProgramWithStdRandom n' [] [RESET]
      assert $ qstate == groundState n'


-- Run tests.

return []
main =
  do
    success <- $quickCheckAll
    if success
      then exitSuccess
      else exitFailure
