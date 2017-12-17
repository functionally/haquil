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


i = scalar $ 0 :+ 1


b0 = rawWavefunction $ qubits [1,0]
b1 = rawWavefunction $ qubits [0,1]

b00 = rawWavefunction $ qubits [1,0 , 0,0]
b01 = rawWavefunction $ qubits [0,1 , 0,0]
b10 = rawWavefunction $ qubits [0,0 , 1,0]
b11 = rawWavefunction $ qubits [0,0 , 0,1]

gg = (rawWavefunction .) . (. (qubits . V.toList . coords)) . (^*)

cos' = scalar . (:+ 0) . cos
sin' = scalar . (:+ 0) . sin
cis' = scalar . cis

va ~~ vb =
  and
    $ zipWith (\a b -> magnitude (a - b) < 1e-5)
    (V.toList $ coords va) (V.toList $ coords vb)
infix 4 ~~


prop_gate_i =
  let
    g = gg $ G.i 0
  in
       g b0 ~~ b0
    && g b1 ~~ b1

prop_gate_x =
  let
    g = gg $ G.x 0
  in
       g b0 ~~ b1
    && g b1 ~~ b0

prop_gate_y =
  let
    g = gg $ G.y 0
  in
       g b0 ~~ i .* b1
    && g b1 ~~ - i .* b0

prop_gate_z =
  let
    g = gg $ G.z 0
  in
       g b0 ~~ b0
    && g b1 ~~ - b1

prop_gate_h =
  let
    g = gg $ G.h 0
  in
       g b0 ~~ (b0 + b1) / sqrt 2
    && g b1 ~~ (b0 - b1) / sqrt 2

prop_gate_phase theta =
  let
    g = gg $ G.phase theta 0
  in
       g b0 ~~ b0
    && g b1 ~~ cis' theta .* b1

prop_gate_s =
  let
    g = gg $ G.s 0
  in
       g b0 ~~ b0
    && g b1 ~~ i .* b1

prop_gate_t =
  let
    g = gg $ G.t 0
  in
       g b0 ~~ b0
    && g b1 ~~ scalar (1 :+ 1) .* b1 / sqrt 2

prop_gate_rx theta =
  let
    g = gg $ G.rx theta 0
    theta' = theta / 2
  in
       g b0 ~~   cos' theta'      .* b0 - sin' theta' .* i .* b1
    && g b1 ~~ - sin' theta' .* i .* b0 + cos' theta'      .* b1

prop_gate_ry theta =
  let
    g = gg $ G.ry theta 0
    theta' = theta / 2
  in
       g b0 ~~   cos' theta' .* b0 + sin' theta' .* b1
    && g b1 ~~ - sin' theta' .* b0 + cos' theta' .* b1

prop_gate_rz theta =
  let
    g = gg $ G.rz theta 0
    theta' = theta / 2
  in
       g b0 ~~ cis' (- theta') .* b0
    && g b1 ~~ cis'    theta'  .* b1


return []
main =
  do
    success <- $quickCheckAll
    if success
      then exitSuccess
      else exitFailure
