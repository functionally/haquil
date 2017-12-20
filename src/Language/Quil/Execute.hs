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


module Language.Quil.Execute (
  compileGate
, compileExpression
) where


import Data.Complex (Complex((:+)))
import Data.Function (on)
import Data.Qubit (Operator, qubitsOperator)
import Language.Quil.Types (Arguments, Expression(..), Number, Parameters, QBit)

import qualified Data.Vector as V ((!), elemIndex)


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
