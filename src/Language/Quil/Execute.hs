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
-- | Executing Quil programs.
--
-- This example makes measurements on a prepared state.
--
-- > >>> import Language.Quil.Execute
-- > >>> import Language.Quil.Types
-- >
-- > -- Run a simple program and examine the quantum and classical states..
-- > >>> let program = [H 0, CNOT 0 1, RX (Expression . Number $ pi/4) 1]
-- > >>> runProgramWithStdRandom 2 [BoolBit False] program
-- > Quantum state:   0.6532814824381882|00> + -0.27059805007309845i|01> + -0.27059805007309845i|10> + 0.6532814824381882|11> @ [1,0]
-- > Classical state: 0x0 [1]
-- > Program counter: 3
-- > Halted?          False
-- >
-- > -- Add measurement to the program and re-run it.
-- > >>> let program' = program ++ [MEASURE 0 (Just 0)]
-- > >>> runProgramWithStdRandom 2 [BoolBit False] program'
-- > Quantum state:   0.9238795325112867|00> + -0.3826834323650897i|10> @ [1,0]
-- > Classical state: 0x0 [1]
-- > Program counter: 4
-- > Halted?          False
-- >
-- > -- Run the program again.
-- > >>> runProgramWithStdRandom 2 [BoolBit False] program'
-- > Quantum state:   -0.3826834323650897i|01> + 0.9238795325112867|11> @ [1,0]
-- > Classical state: 0x1 [1]
-- > Program counter: 4
-- > Halted?          False
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}


module Language.Quil.Execute (
-- * Execution
  runProgram
, runProgramWithStdGen
, runProgramWithStdRandom
, executeInstructions
, executeInstruction
-- * Compilation
, compileGate
, compileExpression
) where


import Control.Monad (foldM, join)
import Control.Monad.Random.Lazy (Rand, RandomGen, evalRand, evalRandIO)
import Data.Bits (complementBit, setBit, testBit)
import Data.BitVector (BV, size)
import Data.Complex (Complex((:+)))
import Data.Function (on)
import Data.Qubit (Operator, Wavefunction, (^*), groundState, measure, qubitsOperator, wavefunctionOrder)
import Language.Quil.Types (Arguments, BitData, Expression(..), Instruction(..), Machine(..), Number, Parameter(..), Parameters, QBit, complexFromBitVector, doubleFromBitVector)
import System.Random (StdGen)

import qualified Data.Qubit.Gate as G
import qualified Data.Vector as V ((!), elemIndex, empty)
import qualified Language.Quil.Types as Q (machine)


-- | Create an action to run a program, starting from the ground state.
runProgramWithStdGen :: StdGen        -- ^ The random number generator.
                     -> Int           -- ^ The number of qubits.
                     -> [BitData]     -- ^ The classical bits.
                     -> [Instruction] -- ^ The instructions.
                     -> Machine       -- ^ The resulting state of the machine.
runProgramWithStdGen stdGen n cstate' instructions = runProgram n cstate' instructions `evalRand` stdGen


-- | Run a program, starting from the ground state and using a particular random-number generator.
runProgramWithStdRandom :: Int           -- ^ The number of qubits.
                        -> [BitData]     -- ^ The classical bits.
                        -> [Instruction] -- ^ The instructions.
                        -> IO Machine    -- ^ Action for the resulting state of the machine.
runProgramWithStdRandom n cstate' instructions = evalRandIO $ runProgram n cstate' instructions


-- | Run a program, starting from the ground state and using the global random number generator.
runProgram :: RandomGen g
           => Int            -- ^ The number of qubits.
           -> [BitData]      -- ^ The classical bits.
           -> [Instruction]  -- ^ The instructions.
           -> Rand g Machine -- ^ Action for the resulting state of the machine.
runProgram n cstate' instructions = executeInstructions instructions $ Q.machine n cstate'


-- | Execute a series of instructions.
executeInstructions :: RandomGen g
                    => [Instruction]  -- ^ The instructions.
                    -> Machine        -- ^ The state of the machine.
                    -> Rand g Machine -- ^ Action for the resulting state of the machine.
executeInstructions = flip $ foldM (flip executeInstruction)


-- | Execute an instruction.
executeInstruction :: RandomGen g
                   => Instruction    -- ^ The instruction.
                   -> Machine        -- ^ The state of the machine.
                   -> Rand g Machine -- ^ Action for the resulting state of the machine.

executeInstruction (COMMENT _) = return

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

executeInstruction (PHASE theta index) = onQubits' (\theta' -> (G.phase (ensureReal theta') index ^*)) theta
executeInstruction (RX    theta index) = onQubits' (\theta' -> (G.rx    (ensureReal theta') index ^*)) theta
executeInstruction (RY    theta index) = onQubits' (\theta' -> (G.ry    (ensureReal theta') index ^*)) theta
executeInstruction (RZ    theta index) = onQubits' (\theta' -> (G.rz    (ensureReal theta') index ^*)) theta

executeInstruction (CPHASE00 theta index index') = onQubits' (\theta' -> (G.cphase00 (ensureReal theta') index index' ^*)) theta
executeInstruction (CPHASE01 theta index index') = onQubits' (\theta' -> (G.cphase01 (ensureReal theta') index index' ^*)) theta
executeInstruction (CPHASE10 theta index index') = onQubits' (\theta' -> (G.cphase10 (ensureReal theta') index index' ^*)) theta
executeInstruction (CPHASE   theta index index') = onQubits' (\theta' -> (G.cphase   (ensureReal theta') index index' ^*)) theta
executeInstruction (PSWAP    theta index index') = onQubits' (\theta' -> (G.pswap    (ensureReal theta') index index' ^*)) theta

executeInstruction DEFGATE    {} = unimplemented "DEFGATE"
executeInstruction USEGATE    {} = unimplemented "DEFGATE"
executeInstruction DEFCIRCUIT {} = unimplemented "DEFCIRCUIT"
executeInstruction USECIRCUIT {} = unimplemented "DEFCIRCUIT"

executeInstruction (MEASURE index address) =
  \machine@Machine{..} ->
    do
      ([(_, b)], qstate') <- measure [index] qstate
      noop
        machine
          {
            qstate = qstate'
          , cstate = maybe id (setBit' (toEnum $ fromEnum b)) address cstate
          }

executeInstruction HALT = fmap (\machine -> machine {halted = True}) . noop
executeInstruction WAIT = noop

executeInstruction LABEL       {} = unimplemented "LABEL"
executeInstruction JUMP        {} = unimplemented "JUMP"
executeInstruction JUMP_WHEN   {} = unimplemented "JUMP_WHEN"
executeInstruction JUMP_UNLESS {} = unimplemented "JUMP_UNLESS"

executeInstruction (FALSE address) = onBits (setBit' False address)
executeInstruction (TRUE  address) = onBits (setBit' True  address)
executeInstruction (NOT   address) = onBits (`complementBit` address)

executeInstruction (AND      address address') = onBits (\bv -> setBit' (bv `testBit` address && bv `testBit` address') address' bv)
executeInstruction (OR       address address') = onBits (\bv -> setBit' (bv `testBit` address || bv `testBit` address') address' bv)
executeInstruction (MOVE     address address') = onBits (\bv -> setBit' (bv `testBit` address                         ) address' bv)
executeInstruction (EXCHANGE address address') = onBits (\bv -> setBit' (bv `testBit` address) address' $ setBit' (bv `testBit` address') address bv)

executeInstruction NOP = noop

executeInstruction (INCLUDE _) = unimplemented "INCLUDE"
executeInstruction (PRAGMA  _) = noop


-- | Ensure a complex number is real.
ensureReal :: Number -- ^ The number.
           -> Double -- ^ The double, or an error.
ensureReal (x :+ 0) = x
ensureReal _        = error "Built-in gate must have real argument."


-- | Report that an instruction has not yet been implemented.
unimplemented :: String -- ^ The name of the instruction.
              -> a      -- ^ THe error.
unimplemented instruction = error $ "The " ++ instruction ++ " instruction has not yet been implemented."


-- | Just increment the progrma counter.
noop :: RandomGen g
     => Machine        -- ^ The machine.
     -> Rand g Machine -- ^ Action for the resulting state of the machine.
noop machine@Machine{..} =
  if halted
    then error "The machine has halted."
    else return machine { counter = counter + 1 }


-- | Transform the wafefunction.
onQubits :: RandomGen g
         => (Wavefunction -> Wavefunction) -- ^ The transformation of the wavefunction.
         -> Machine                        -- ^ The machine.
         -> Rand g Machine                 -- ^ Action for the resulting state of the machine.
onQubits transition machine@Machine{..} = noop machine { qstate  = transition qstate }


-- | Transofrm the wavefunction.
onQubits' :: RandomGen g
          => (Number -> Wavefunction -> Wavefunction) -- ^ The parameterized transformation of the wavefunction.
          -> Parameter                                -- ^ The parameter.
          -> Machine                                  -- ^ The machine.
          -> Rand g Machine                           -- ^ Action for the resulting state of the machine.
onQubits' _ (DynamicParameter _ Nothing) _ = error "A single bit cannot be used as an input parameter for a gate."
onQubits' transition (DynamicParameter address (Just address')) machine@Machine{..}
  | address + 63  == address' = onQubits (transition $ doubleFromBitVector  address cstate :+ 0) machine
  | address + 127 == address' = onQubits (transition $ complexFromBitVector address cstate     ) machine
  | otherwise                 = error "A number must consist of 64 or 128 consecutive bits."
onQubits' transition (Expression expression) machine = onQubits (transition $ compileExpression V.empty expression V.empty) machine


-- Transform the classical bis.
onBits :: RandomGen g
       => (BV -> BV)     -- ^ The transformation of the bits.
       -> Machine        -- ^ THe machine.
       -> Rand g Machine -- ^ Action for the resulting state of the machine.
onBits transition machine@Machine{..} =
  let
    cstate' = transition cstate
  in
    if size cstate == size cstate'
      then noop machine { cstate = transition cstate }
      else error "Cannot enlarge tne number of classical bits."


-- | Set a bit.
setBit' :: Bool -- ^ The value.
        -> Int  -- ^ Which bit.
        -> BV   -- ^ The initial bits.
        -> BV   -- ^ The result.
setBit' True  = flip setBit
setBit' False = flip (join . (complementBit .) . setBit) -- See <https://bwbush.atlassian.net/browse/HQUIL-7>.


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
                  -> Arguments  -- ^ The arguments.
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
