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
-- | Testing qbits and operations on them.
--
-- The test data is derived from pyQuil \<<http://pyquil.readthedocs.io>\ and recorded in \<<https://bitbucket.org/functionally/quil/src/99a2efb54fd2c7ac99a9bde3091da130bdadbf6b/test-data.ipynb>\>.
-----------------------------------------------------------------------------


{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module Main (
  main
) where


import Data.Complex (Complex(..), cis, magnitude)
import Data.Qubit ((^*), qubits, rawWavefunction)
import Numeric.LinearAlgebra.Array ((.*))
import Numeric.LinearAlgebra.Array.Util (coords, scalar)
import Test.QuickCheck.All (quickCheckAll)
import System.Exit (exitFailure, exitSuccess)

import qualified Data.Qubit.Gate as G
import qualified Data.Vector.Storable as V (toList)


-- Helper functions.

i = scalar $ 0 :+ 1

cos' = scalar . (:+ 0) . cos
sin' = scalar . (:+ 0) . sin
cis' = scalar . cis

applyGate = (rawWavefunction .) . (. (qubits . V.toList . coords)) . (^*)

epsilon = 1e-5

xs ~~ ys =
  and
    $ zipWith (\x y -> magnitude (x - y) <= epsilon)
    (V.toList $ coords xs) (V.toList $ coords ys)
infix 4 ~~


-- Tests for one-qubit gates.

b0 = rawWavefunction $ qubits [1,0]
b1 = rawWavefunction $ qubits [0,1]

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

b00 = rawWavefunction $ qubits [1,0 , 0,0]
b10 = rawWavefunction $ qubits [0,1 , 0,0]
b01 = rawWavefunction $ qubits [0,0 , 1,0]
b11 = rawWavefunction $ qubits [0,0 , 0,1]

prop_gate_cphase00 theta =
  let
    g = applyGate $ G.cphase00 theta 0 1
  in
       g b00 ~~ cis' theta .* b00
    && g b10 ~~ b10
    && g b01 ~~ b01
    && g b11 ~~ b11

prop_gate_cphase01 theta =
  let
    g = applyGate $ G.cphase01 theta 0 1
  in
       g b00 ~~ b00
    && g b10 ~~ cis' theta .* b10
    && g b01 ~~ b01
    && g b11 ~~ b11

prop_gate_cphase10 theta =
  let
    g = applyGate $ G.cphase10 theta 0 1
  in
       g b00 ~~ b00
    && g b10 ~~ b10
    && g b01 ~~ cis' theta .* b01
    && g b11 ~~ b11

prop_gate_cphase theta =
  let
    g = applyGate $ G.cphase theta 0 1
  in
       g b00 ~~ b00
    && g b10 ~~ b10
    && g b01 ~~ b01
    && g b11 ~~ cis' theta .* b11

prop_gate_cnot =
  let
    g = applyGate $ G.cnot 0 1
  in
       g b00 ~~ b00
    && g b10 ~~ b10
    && g b01 ~~ b11
    && g b11 ~~ b01

prop_gate_pswap theta =
  let
    g = applyGate $ G.pswap theta 0 1
  in
       g b00 ~~ b00
    && g b10 ~~ cis' theta .* b01
    && g b01 ~~ cis' theta .* b10
    && g b11 ~~ b11

prop_gate_swap =
  let
    g = applyGate $ G.swap 0 1
  in
       g b00 ~~ b00
    && g b10 ~~ b01
    && g b01 ~~ b10
    && g b11 ~~ b11

prop_gate_iswap =
  let
    g = applyGate $ G.iswap 0 1
  in
       g b00 ~~ b00
    && g b10 ~~ i .* b01
    && g b01 ~~ i .* b10
    && g b11 ~~ b11

prop_gate_cz =
  let
    g = applyGate $ G.cz 0 1
  in
       g b00 ~~ b00
    && g b10 ~~ b10
    && g b01 ~~ b01
    && g b11 ~~ - b11


-- Tests for three-cubit gates.

b000 = rawWavefunction $ qubits [1,0 , 0,0 , 0,0 , 0,0]
b100 = rawWavefunction $ qubits [0,1 , 0,0 , 0,0 , 0,0]
b010 = rawWavefunction $ qubits [0,0 , 1,0 , 0,0 , 0,0]
b110 = rawWavefunction $ qubits [0,0 , 0,1 , 0,0 , 0,0]
b001 = rawWavefunction $ qubits [0,0 , 0,0 , 1,0 , 0,0]
b101 = rawWavefunction $ qubits [0,0 , 0,0 , 0,1 , 0,0]
b011 = rawWavefunction $ qubits [0,0 , 0,0 , 0,0 , 1,0]
b111 = rawWavefunction $ qubits [0,0 , 0,0 , 0,0 , 0,1]

prop_gate_ccnot =
  let
    g = applyGate $ G.ccnot 0 1 2
  in
       g b000 ~~ b000
    && g b100 ~~ b100
    && g b010 ~~ b010
    && g b110 ~~ b110
    && g b001 ~~ b001
    && g b101 ~~ b101
    && g b011 ~~ b111
    && g b111 ~~ b011

prop_gate_cswap =
  let
    g = applyGate $ G.cswap 0 1 2
  in
       g b000 ~~ b000
    && g b100 ~~ b100
    && g b010 ~~ b010
    && g b110 ~~ b110
    && g b001 ~~ b001
    && g b101 ~~ b011
    && g b011 ~~ b101
    && g b111 ~~ b111


-- Run tests.

return []
main =
  do
    success <- $quickCheckAll
    if success
      then exitSuccess
      else exitFailure
