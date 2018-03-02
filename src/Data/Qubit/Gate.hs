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
-- | Gates for qubit wavefunctions, mostly as in Quil \<<https://arxiv.org/abs/1608.03355/\>> and using the conventions of \<<https://arxiv.org/abs/1711.02086/>\>.
--
-----------------------------------------------------------------------------


module Data.Qubit.Gate (
-- * One-Cubit Gates
  i
, x
, y
, z
, h
, s
, t
-- * Parameterized One-Cubit Gates
, phase
, rx
, ry
, rz
-- * Two-Cubit Gates
, cnot
, swap
, iswap
-- * Parameterized Two-Cubit Gates
, cphase00
, cphase01
, cphase10
, cphase
, pswap
, cz
-- * Three-Cubit Gates
, ccnot
, cswap
) where


import Data.Complex (Complex(..), cis)
import Data.Qubit (Operator, QIndex, qubitsOperator)


-- | The identity gate. This corresponds to I in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
i :: QIndex   -- ^ The index of the qubit in the wavefunction.
  -> Operator -- ^ The operator.
i i0 =
  qubitsOperator [i0]
    [
      1, 0
    , 0, 1
    ]


-- | The Pauli-x gate. This corresponds to X in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
x :: QIndex   -- ^ The index of the qubit in the wavefunction.
  -> Operator -- ^ The operator.
x i0 =
  qubitsOperator [i0]
    [
      0, 1
    , 1, 0
    ]


-- | The Pauli-y gate. This corresponds to Y in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
y :: QIndex   -- ^ The index of the qubit in the wavefunction.
  -> Operator -- ^ The operator.
y i0  =
  qubitsOperator [i0]
    [
      0     , 0 :+ (-1)
    , 0 :+ 1, 0
    ]


-- | The Pauli-z gate. This corresponds to Z in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
z :: QIndex   -- ^ The index of the qubit in the wavefunction.
  -> Operator -- ^ The operator.
z i0 =
  qubitsOperator [i0]
    [
      1, 0
    , 0, -1
    ]


-- | The Hadamard gate. This corresponds to H in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
h :: QIndex   -- ^ The index of the qubit in the wavefunction.
  -> Operator -- ^ The operator.
h i0 =
  qubitsOperator [i0]
    [
      1, 1
    , 1, -1
    ] / sqrt 2


-- | The phase-shift gate. This corresponds to PHASE in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
phase :: Double   -- ^ The phase angle [radians].
      -> QIndex   -- ^ The index of the qubit in the wavefunction.
      -> Operator -- ^ The operator.
phase  theta i0 =
  qubitsOperator [i0]
    [
      1, 0
    , 0, cis theta
    ]


-- | The one-qubit 90-degree phase-shift gate. This corresponds to S in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
s :: QIndex   -- ^ The index of the qubit in the wavefunction.
  -> Operator -- ^ The operator.
s i0 =
  qubitsOperator [i0]
    [
      1, 0
    , 0, 0 :+ 1
    ]


-- | The one-qubit 45-degree phase-shift gate. This corresponds to T in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
t :: QIndex   -- ^ The index of the qubit in the wavefunction.
  -> Operator -- ^ The operator.
t i0 =
  qubitsOperator [i0]
    [
      1, 0
    , 0, (1 :+ 1) / sqrt 2
    ]


-- | A controlled phase gate on |00>. This corresponds to CPHASE00 in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
cphase00 :: Double   -- ^ The phase angle [radians].
         -> QIndex   -- ^ The index of the higher qubit in the wavefunction.
         -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
         -> Operator -- ^ The operator.
cphase00 theta i0 i1 =
  qubitsOperator [i0, i1]
    [
      cis theta, 0, 0, 0
    , 0        , 1, 0, 0
    , 0        , 0, 1, 0
    , 0        , 0, 0, 1
    ]


-- | A controlled phase gate on |01>. This corresponds to CPHASE01 in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
cphase01 :: Double   -- ^ The phase angle [radians].
         -> QIndex   -- ^ The index of the higher qubit in the wavefunction.
         -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
         -> Operator -- ^ The operator.
cphase01 theta i0 i1 =
  qubitsOperator [i0, i1]
    [
      1, 0        , 0, 0
    , 0, cis theta, 0, 0
    , 0, 0        , 1, 0
    , 0, 0        , 0, 1
    ]


-- | A conrolled pahse gate on |10>. This corresponds to CPHASE10 in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
cphase10 :: Double   -- ^ The phase angle [radians].
         -> QIndex   -- ^ The index of the higher qubit in the wavefunction.
         -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
         -> Operator -- ^ The operator.
cphase10 theta i0 i1 =
  qubitsOperator [i0, i1]
    [
      1, 0, 0        , 0
    , 0, 1, 0        , 0
    , 0, 0, cis theta, 0
    , 0, 0, 0        , 1
    ]


-- | A controlled phase gate on |11>. This corresponds to CPHASE in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
cphase :: Double   -- ^ The phase angle [radians].
       -> QIndex   -- ^ The index of the higher qubit in the wavefunction.
       -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
       -> Operator -- ^ The operator.
cphase theta i0 i1 =
  qubitsOperator [i0, i1]
    [
      1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , 0, 0, 0, cis theta
    ]


-- | The x-rotation gate. This corresponds to RX in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
rx :: Double   -- ^ The phase angle [radians].
   -> QIndex   -- ^ The index of the qubit in the wavefunction.
   -> Operator -- ^ The operator.
rx theta i0 =
  qubitsOperator [i0]
    [
      cos (theta / 2) :+ 0        , 0 :+ (- sin (theta / 2))
    , 0 :+ (- sin (theta / 2)), cos (theta / 2) :+ 0
    ]


-- | The y-rotation gate. This corresponds to RY in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
ry :: Double   -- ^ The phase angle [radians].
   -> QIndex   -- ^ The index of the qubit in the wavefunction.
   -> Operator -- ^ The operator.
ry theta i0 =
  qubitsOperator [i0]
    [
      cos (theta / 2) :+ 0, (- sin (theta / 2)) :+ 0
    , sin (theta / 2) :+ 0, cos (theta / 2) :+ 0
    ]


-- | The z-rotation gate. This corresponds to RZ in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
rz :: Double   -- ^ The phase angle [radians].
   -> QIndex   -- ^ The index of the qubit in the wavefunction.
   -> Operator -- ^ The operator.
rz theta i0 =
  qubitsOperator [i0]
    [
      cis (- theta / 2), 0
    , 0                , cis (theta / 2)
    ]


-- | The controlled-not gate for two qubits. This corresponds to CNOT in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
cnot :: QIndex   -- ^ The index of the higher qubit in the wavefunction.
     -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
     -> Operator -- ^ The operator.
cnot i0 i1 =
  qubitsOperator [i0, i1]
    [
      1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 0, 1
    , 0, 0, 1, 0
    ]


-- | The controlled-not gate by three qubits. This corresponds to CCNOT in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
ccnot :: QIndex   -- ^ The index of the higher qubit in the wavefunction.
      -> QIndex   -- ^ The index of the middle qubit in the wavefunction.
      -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
      -> Operator -- ^ The operator.
ccnot i0 i1 i2 =
  qubitsOperator [i0, i1, i2]
    [
      1, 0, 0, 0, 0, 0, 0, 0
    , 0, 1, 0, 0, 0, 0, 0, 0
    , 0, 0, 1, 0, 0, 0, 0, 0
    , 0, 0, 0, 1, 0, 0, 0, 0
    , 0, 0, 0, 0, 1, 0, 0, 0
    , 0, 0, 0, 0, 0, 1, 0, 0
    , 0, 0, 0, 0, 0, 0, 0, 1
    , 0, 0, 0, 0, 0, 0, 1, 0
    ]


-- | The phase-swap gate. This corresponds to PSWAP in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
pswap :: Double   -- ^ The phase angle [radians].
      -> QIndex   -- ^ The index of the higher qubit in the wavefunction.
      -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
      -> Operator -- ^ The operator.
pswap theta i0 i1 =
  qubitsOperator [i0, i1]
    [
      1, 0        , 0        , 0
    , 0, 0        , cis theta, 0
    , 0, cis theta, 0        , 0
    , 0, 0        , 0        , 1
    ]
   

-- | The swap gate with 0-degree phase. This corresponds to SWAP in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
swap :: QIndex   -- ^ The index of the higher qubit in the wavefunction.
     -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
     -> Operator -- ^ The operator.
swap i0 i1 =
  qubitsOperator [i0, i1]
    [
      1, 0, 0, 0
    , 0, 0, 1, 0
    , 0, 1, 0, 0
    , 0, 0, 0, 1
    ]

-- | The swap gate with 90-degree phase. This corresponds to ISWAP in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
iswap :: QIndex   -- ^ The index of the higher qubit in the wavefunction.
      -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
      -> Operator -- ^ The operator.
iswap i0 i1 =
  qubitsOperator [i0, i1]
    [
      1, 0     , 0     , 0
    , 0, 0     , 0 :+ 1, 0
    , 0, 0 :+ 1, 0     , 0
    , 0, 0     , 0     , 1
    ]

-- | The three-qubit controlled-swap gate. This corresponds to CSWAP in Quil \<<https://arxiv.org/abs/1608.03355/\>>, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
cswap :: QIndex   -- ^ The index of the higher qubit in the wavefunction.
      -> QIndex   -- ^ The index of the middle qubit in the wavefunction.
      -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
      -> Operator -- ^ The operator.
cswap i0 i1 i2 =
  qubitsOperator [i0, i1, i2]
    [
      1, 0, 0, 0, 0, 0, 0, 0
    , 0, 1, 0, 0, 0, 0, 0, 0
    , 0, 0, 1, 0, 0, 0, 0, 0
    , 0, 0, 0, 1, 0, 0, 0, 0
    , 0, 0, 0, 0, 1, 0, 0, 0
    , 0, 0, 0, 0, 0, 0, 1, 0
    , 0, 0, 0, 0, 0, 1, 0, 0
    , 0, 0, 0, 0, 0, 0, 0, 1
    ]
    

-- | The controlled-z gate, with the indexing convention of \<<https://arxiv.org/pdf/1711.02086/>\>.
cz :: QIndex   -- ^ The index of the higher qubit in the wavefunction.
   -> QIndex   -- ^ The index of the lower qubit in the wavefunction.
   -> Operator -- ^ The operator.
cz i0 i1 =
  qubitsOperator [i0, i1]
    [
      1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , 0, 0, 0, -1
    ]
