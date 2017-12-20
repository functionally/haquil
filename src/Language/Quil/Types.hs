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
-- | Types for the  Quil language for quantum computing, \<<https://arxiv.org/abs/1608.03355/>\>, and generally conforming to \<<https://github.com/rigetticomputing/pyquil/blob/master/pyquil/_parser/Quil.g4>\>.
--
-----------------------------------------------------------------------------


module Language.Quil.Types (
-- * Machines
  Machine(..)
, Definitions(..)
, Gate
, Circuit
-- * Instructions
, Instruction(..)
, CircuitInstruction(..)
, CircuitQBit(..)
, Parameter(..)
, Name
, QBit
, QVariable
, Address
, Variable
, Label
-- * Expressions
, Expression(..)
, Number
, Parameters
, Arguments
) where


import Data.Complex (Complex)
import Data.Qubit (Operator, Wavefunction)
import Data.Vector (Vector)


-- | A quantum abstract machine.
data Machine =
  Machine
  {
    qstate       :: Wavefunction      -- ^ The qubits.
  , cstate       :: Integer           -- ^ The classical bits
  , definitions  :: Definitions       -- ^ Definitions of gates and circuits.
  , instructions :: [Instruction]     -- ^ The program instructions.
  , counter      :: Int               -- ^ The program counter.
  }


-- | Definitions of gates and circuits.
data Definitions =
  Definitions
  {
    gates    :: [(Name, Gate)]
  , circuits :: [(Name, Circuit)]
  }


-- | A gate.
type Gate = [Parameter] -> [QBit] -> Operator


-- | A circuit
type Circuit = Definitions -> [Parameter] -> [QBit] -> Operator


-- | The Quil instruction set.
data Instruction =
    COMMENT String
  | RESET
  | I QBit
  | X QBit
  | Y QBit
  | Z QBit
  | H QBit
  | PHASE Parameter QBit
  | S QBit
  | T QBit
  | CPHASE00 Parameter QBit
  | CPHASE01 Parameter QBit
  | CPHASE10 Parameter QBit
  | CPHASE Parameter QBit
  | RX Parameter QBit
  | RY Parameter QBit
  | RZ Parameter QBit
  | CNOT QBit QBit
  | CCNOT QBit QBit QBit
  | PSWAP Parameter QBit QBit
  | SWAP QBit QBit
  | ISWAP QBit QBit
  | CSWAP QBit QBit QBit
  | CZ QBit
  | DEFGATE Name [Variable] [Expression] 
  | USEGATE Name [Parameter] [QBit]
  | DEFCIRCUIT Name [Variable] [QVariable] [CircuitInstruction]
  | USECIRCUIT Name [Parameter] [QBit]
  | MEASURE [QBit] (Maybe Address)
  | HALT
  | WAIT
  | LABEL Label
  | JUMP Label
  | JUMP_WHEN Label Address
  | JUMP_UNLESS Label Address
  | FALSE Address
  | TRUE Address
  | NOT Address
  | AND Address Address
  | OR Address Address
  | MOVE Address Address
  | EXCHANGE Address Address
  | NOP
  | INCLUDE FilePath
  | PRAGMA String
    deriving (Eq, Read, Show)


-- | Instructions within circuit definitions.
data CircuitInstruction =
    CircuitInstruction Instruction
  | CircuitGate Name [Parameter] [CircuitQBit]
    deriving (Eq, Read, Show)


-- | References to qubits within circuit definitions.
data CircuitQBit =
    CircuitQBit QBit
  | CircuitQVariable QVariable
    deriving (Eq, Read, Show)


-- | Classical parameter.
data Parameter =
    DynamicParameter Int (Maybe Int)
  | Expression Expression
    deriving (Eq, Read, Show)


-- | Name of a gate or circuit.
type Name = String


-- | Index of a qubit.
type QBit = Int


-- | Qubit variable name.
type QVariable = String


-- | Address of a classical bit.
type Address = Int


-- | Classical variable name.
type Variable = String


-- | Label for a jump target.
type Label = String


-- | Classical expression.
data Expression =
    Power Expression Expression
  | Times Expression Expression
  | Divide Expression Expression
  | Plus Expression Expression
  | Minus Expression Expression
  | Negate Expression
  | Sin Expression
  | Cos Expression
  | Sqrt Expression
  | Exp Expression
  | Cis Expression
  | Number Number
  | Variable Variable
    deriving (Eq, Read, Show)


-- | Complex number.
type Number = Complex Double


-- | Formal parameters.
type Parameters = Vector Variable


-- | Argument list.
type Arguments = Vector Number
