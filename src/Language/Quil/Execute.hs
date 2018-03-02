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
-- | Executing Quil programs.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}


module Language.Quil.Execute (
  executeInstruction
, compileGate
, compileExpression
) where


import Data.Bits (clearBit, complementBit, setBit, testBit)
import Data.BitVector (BV)
import Data.Complex (Complex((:+)))
import Data.Function (on)
import Data.Qubit (Operator, Wavefunction, (^*), groundState, qubitsOperator, wavefunctionOrder)
import Language.Quil.Types (Arguments, Expression(..), Instruction(..), Machine(..), Number, Parameter(..), Parameters, QBit)

import qualified Data.Qubit.Gate as G
import qualified Data.Vector as V ((!), elemIndex)


-- | Execute an instruction.
executeInstruction :: Instruction -- ^ The instruction.
                   -> Machine     -- ^ The state of the machine.
                   -> Machine     -- ^ The resulting state of the machine.

executeInstruction (COMMENT _) = id

executeInstruction RESET = onQubits $ groundState . wavefunctionOrder

executeInstruction (I index) = onQubits (G.i index ^*)
executeInstruction (X index) = onQubits (G.x index ^*)
executeInstruction (Y index) = onQubits (G.y index ^*)
executeInstruction (Z index) = onQubits (G.z index ^*)
executeInstruction (H index) = onQubits (G.h index ^*)
executeInstruction (S index) = onQubits (G.s index ^*)
executeInstruction (T index) = onQubits (G.t index ^*)

executeInstruction (CNOT  index index') = onQubits (G.cnot  index index' ^*)
executeInstruction (SWAP  index index') = onQubits (G.swap  index index' ^*)
executeInstruction (ISWAP index index') = onQubits (G.iswap index index' ^*)
executeInstruction (CZ    index index') = onQubits (G.cz    index index' ^*)

executeInstruction (CCNOT index index' index'') = onQubits (G.ccnot index index' index'' ^*)
executeInstruction (CSWAP index index' index'') = onQubits (G.cswap index index' index'' ^*)

executeInstruction (PHASE theta index) = onQubits' (\theta' -> (G.phase theta' index ^*)) theta
executeInstruction (RX    theta index) = onQubits' (\theta' -> (G.rx    theta' index ^*)) theta
executeInstruction (RY    theta index) = onQubits' (\theta' -> (G.ry    theta' index ^*)) theta
executeInstruction (RZ    theta index) = onQubits' (\theta' -> (G.rz    theta' index ^*)) theta

executeInstruction (CPHASE00 theta index index') = onQubits' (\theta' -> (G.cphase00 theta' index index' ^*)) theta
executeInstruction (CPHASE01 theta index index') = onQubits' (\theta' -> (G.cphase01 theta' index index' ^*)) theta
executeInstruction (CPHASE10 theta index index') = onQubits' (\theta' -> (G.cphase10 theta' index index' ^*)) theta
executeInstruction (CPHASE   theta index index') = onQubits' (\theta' -> (G.cphase   theta' index index' ^*)) theta
executeInstruction (PSWAP    theta index index') = onQubits' (\theta' -> (G.pswap    theta' index index' ^*)) theta

executeInstruction (DEFGATE    _ _ _  ) = error "The DEFGATE instruction has not been implemented."
executeInstruction (USEGATE    _ _ _  ) = error "The DEFGATE instruction has not been implemented."
executeInstruction (DEFCIRCUIT _ _ _ _) = error "The DEFCIRCUIT instruction has not been implemented."
executeInstruction (USECIRCUIT _ _ _  ) = error "The DEFCIRCUIT instruction has not been implemented."

executeInstruction (MEASURE _ _) = undefined

executeInstruction HALT = \machine -> machine {halted = True}
executeInstruction WAIT = onBits id

executeInstruction (LABEL       _  ) = error "The LABEL instruction has not been implemented."
executeInstruction (JUMP        _  ) = error "The JUMP instruction has not been implemented."
executeInstruction (JUMP_WHEN   _ _) = error "The JUMP_WHEN instruction has not been implemented."
executeInstruction (JUMP_UNLESS _ _) = error "The JUMP_UNLESS instruction has not been implemented."

executeInstruction (FALSE address) = onBits (setBit' False address)
executeInstruction (TRUE  address) = onBits (setBit' True  address)
executeInstruction (NOT   address) = onBits (`complementBit` address)

executeInstruction (AND      address address') = onBits (\bv -> setBit' (bv `testBit` address && bv `testBit` address') address' bv)
executeInstruction (OR       address address') = onBits (\bv -> setBit' (bv `testBit` address || bv `testBit` address') address' bv)
executeInstruction (MOVE     address address') = onBits (\bv -> setBit' (bv `testBit` address                         ) address' bv)
executeInstruction (EXCHANGE address address') = onBits (\bv -> setBit' (bv `testBit` address) address' $ setBit' (bv `testBit` address') address bv)

executeInstruction NOP = onBits id

executeInstruction (INCLUDE _) = error "The INCLUDE instruction has not been implemented."
executeInstruction (PRAGMA  _) = onBits id


onQubits :: (Wavefunction -> Wavefunction) -> Machine -> Machine
onQubits transition machine@Machine{..} =
  machine
  {
    qstate  = transition qstate
  , counter = counter + 1
  }


onQubits' :: (Double -> Wavefunction -> Wavefunction) -> Parameter -> Machine -> Machine
onQubits' _transition _parameter _machine@Machine{..} = undefined


onQubits'' :: (Number -> Wavefunction -> Wavefunction) -> Parameter -> Machine -> Machine
onQubits'' _transition _parameter _machine@Machine{..} = undefined


onBits :: (BV -> BV) -> Machine -> Machine
onBits transition machine@Machine{..} =
  machine
  {
    cstate  = transition cstate
  , counter = counter + 1
  }


setBit' :: Bool -> Int -> BV -> BV
setBit' True  = flip setBit
setBit' False = flip clearBit


-- | Compile a gate.
compileGate :: Parameters   -- ^ The formal parameters.
            -> [Expression] -- ^ The expressions for the matrix elements.
            -> [QBit]       -- ^ Which qubits to operate on.
            -> Arguments    -- ^ The argument.
            -> Operator     -- ^ The resulting operator.
compileGate parameters expressions indices arguments =
  qubitsOperator indices
    $ flip (compileExpression parameters) arguments <$> expressions


-- | Compile an expression.
compileExpression :: Parameters -- ^ The formal parameters.
                  -> Expression -- ^ The expression.
                  -> Arguments  -- ^ The argument.
                  -> Number     -- ^ The result.
compileExpression parameters (Power  x y) = \z -> ((**) `on` flip (compileExpression parameters) z) x y
compileExpression parameters (Times  x y) = \z -> ((*)  `on` flip (compileExpression parameters) z) x y
compileExpression parameters (Divide x y) = \z -> ((/)  `on` flip (compileExpression parameters) z) x y
compileExpression parameters (Plus   x y) = \z -> ((+)  `on` flip (compileExpression parameters) z) x y
compileExpression parameters (Minus  x y) = \z -> ((-)  `on` flip (compileExpression parameters) z) x y
compileExpression parameters (Negate   x) = negate . compileExpression parameters x
compileExpression parameters (Sin      x) = sin    . compileExpression parameters x
compileExpression parameters (Cos      x) = cos    . compileExpression parameters x
compileExpression parameters (Sqrt     x) = sqrt   . compileExpression parameters x
compileExpression parameters (Exp      x) = exp    . compileExpression parameters x
compileExpression parameters (Cis      x) = cis    . compileExpression parameters x
                                              where cis z = cos z + (0 :+ 1) * sin z
compileExpression _          (Number   x) = const x
compileExpression parameters (Variable x) = \z ->
                                              maybe
                                                (error $ "Undefined variable \"" ++ x ++ "\".")
                                                (z V.!)
                                                $ x `V.elemIndex` parameters
