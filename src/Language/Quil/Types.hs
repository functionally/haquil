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
-- | Types for the  Quil language for quantum computing, \<<https://arxiv.org/abs/1608.03355/>\>, and generally conforming to \<<https://github.com/rigetticomputing/pyquil/blob/master/pyquil/_parser/Quil.g4>\>.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}


module Language.Quil.Types (
-- * Machines
  Machine(..)
, machine
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
-- * Classical Bits
, BitData(..)
, toBitVector
, boolFromBitVector
, finiteBitsFromBitVector
, integerFromBitVector
, doubleFromBitVector
, complexFromBitVector
) where


import Data.Binary.IEEE754 (doubleToWord, wordToDouble)
import Data.BitVector (BV, bitVec, extract, showHex, testBit)
import Data.Bits (FiniteBits(finiteBitSize))
import Data.Complex (Complex((:+)), imagPart, realPart)
import Data.Default (Default(def))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid ((<>))
import Data.Qubit (Operator, Wavefunction, groundState)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)


-- | A quantum abstract machine.
data Machine =
  Machine
  {
    qstate       :: Wavefunction -- ^ The qubits.
  , cstate       :: BV           -- ^ The classical bits
  , definitions  :: Definitions  -- ^ Definitions of gates and circuits.
  , counter      :: Int          -- ^ The program counter.
  , halted       :: Bool         -- ^ Whether the machine has halted.
  }

instance Show Machine where
  show Machine{..} =
    unlines
      [
        "Quantum state:   " ++ show    qstate
      , "Classical state: " ++ showHex cstate
      , "Program counter: " ++ show    counter
      , "Halted?          " ++ show    halted
      ]

instance Default Machine where
  def = machine 1 [BoolBit False]


-- | Initialize a machine.
machine :: Int       -- ^ The number of qubits.
        -> [BitData] -- ^ The classical bits.
        -> Machine   -- ^ The machine.
machine n cstate' =
  let
    qstate       = groundState n
    cstate       = mconcat $ toBitVector <$> cstate'
    definitions  = def
    counter      = 0
    halted       = False
  in
    Machine{..}


-- | Data types for encoding as bit vectors.
data BitData =
    BoolBit Bool
  | IntBits8 Int8
  | IntBits16 Int16
  | IntBits32 Int32
  | IntBits64 Int64
  | WordBits8 Word8
  | WordBits16 Word16
  | WordBits32 Word32
  | WordBits64 Word64
  | IntegerBits Int Integer
  | DoubleBits Double
  | ComplexBits Number
    deriving (Eq, Read, Show)


-- | Encode data as a bit vector.
toBitVector :: BitData -> BV
toBitVector (BoolBit       x) = bitVec  1 $ fromEnum x
toBitVector (IntBits8      x) = bitVec  8 $ toInteger x
toBitVector (IntBits16     x) = bitVec 16 $ toInteger x
toBitVector (IntBits32     x) = bitVec 32 $ toInteger x
toBitVector (IntBits64     x) = bitVec 64 $ toInteger x
toBitVector (WordBits8     x) = bitVec  8 $ toInteger x
toBitVector (WordBits16    x) = bitVec 16 $ toInteger x
toBitVector (WordBits32    x) = bitVec 32 $ toInteger x
toBitVector (WordBits64    x) = bitVec 64 $ toInteger x
toBitVector (IntegerBits n x) = bitVec  n   x
toBitVector (DoubleBits    x) = bitVec 64 $ doubleToWord x
toBitVector (ComplexBits   x) = bitVec 64   (doubleToWord $ realPart x) <> bitVec 64 (doubleToWord $ imagPart x)


-- | Extract a boolean from a bit vector.
boolFromBitVector :: Int  -- ^ Which bit to start from, counting from zero.
                  -> BV   -- ^ The bit vector.
                  -> Bool -- ^ The boolean.
boolFromBitVector = flip testBit


-- | Extract a finite bits from a bit vector.
finiteBitsFromBitVector :: (Integral a, FiniteBits a)
                        => Int  -- ^ Which bit to start from, counting from zero.
                        -> BV   -- ^ The bit vector.
                        -> a    -- ^ The finite bits.
finiteBitsFromBitVector k v =
  let
    y = fromIntegral . toInteger $ extract (k + finiteBitSize y - 1) k v
  in
    y

-- | Extract an integer from a bit vector.
integerFromBitVector :: Int -- ^ Which bit to start from, counting from zero.
                     -> Int     -- ^ How many bits to encode.
                     -> BV      -- ^ The bit vector
                     -> Integer -- ^ The integer (unsigned).
integerFromBitVector k n = toInteger . extract (k + n - 1) k


-- | Extract a double from a bit vector.
doubleFromBitVector :: Int    -- ^ Which bit to start from, counting from zero.
                    -> BV     -- ^ THe bit vector.
                    -> Double -- ^ The double.
doubleFromBitVector k = wordToDouble . fromIntegral . extract (k + 63) k


-- | Extract a complex number from a bit vector.
complexFromBitVector :: Int -- ^ Which bit to start from, counting from zero.
                     -> BV  -- ^ THe bit vector.
                     -> Number -- ^ The complex number.
complexFromBitVector k x = doubleFromBitVector (k + 64) x :+ doubleFromBitVector k x


-- | Definitions of gates and circuits.
data Definitions =
  Definitions
  {
    gates    :: [(Name, Gate)]
  , circuits :: [(Name, Circuit)]
  }

instance Default Definitions where
  def = Definitions [] []


-- | A gate.
type Gate = [QBit] -> Arguments -> Operator


-- | A circuit
type Circuit = Definitions -> [QBit] -> Arguments -> Operator


-- | The Quil instruction set.
data Instruction =
    COMMENT String -- ^ /Does nothing./
  | RESET
  | I QBit
  | X QBit
  | Y QBit
  | Z QBit
  | H QBit
  | PHASE Parameter QBit
  | S QBit
  | T QBit
  | CPHASE00 Parameter QBit QBit
  | CPHASE01 Parameter QBit QBit
  | CPHASE10 Parameter QBit QBit
  | CPHASE Parameter QBit QBit
  | RX Parameter QBit
  | RY Parameter QBit
  | RZ Parameter QBit
  | CNOT QBit QBit
  | CCNOT QBit QBit QBit
  | PSWAP Parameter QBit QBit
  | SWAP QBit QBit
  | ISWAP QBit QBit
  | CSWAP QBit QBit QBit
  | CZ QBit QBit
  | DEFGATE Name [Variable] [Expression]                        -- ^ /Not yet implemented./
  | USEGATE Name [Parameter] [QBit]                             -- ^ /Not yet implemented./
  | DEFCIRCUIT Name [Variable] [QVariable] [CircuitInstruction] -- ^ /Not yet implemented./
  | USECIRCUIT Name [Parameter] [QBit]                          -- ^ /Not yet implemented./
  | MEASURE QBit (Maybe Address)
  | HALT
  | WAIT                      -- ^ /Does nothing./
  | LABEL Label               -- ^ /Not yet implemented./
  | JUMP Label                -- ^ /Not yet implemented./
  | JUMP_WHEN Label Address   -- ^ /Not yet implemented./
  | JUMP_UNLESS Label Address -- ^ /Not yet implemented./
  | FALSE Address
  | TRUE Address
  | NOT Address
  | AND Address Address
  | OR Address Address
  | MOVE Address Address
  | EXCHANGE Address Address
  | NOP
  | INCLUDE FilePath -- ^ /Not yet implemented./
  | PRAGMA String    -- ^ /Does nothing./
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
