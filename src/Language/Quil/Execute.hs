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
  evaluateExpression
) where


import Data.Complex (Complex((:+)))
import Data.Function (on)
import Language.Quil.Types (Arguments, Expression(..), Number, Parameters)

import qualified Data.Vector as V ((!), elemIndex)


-- | Evaluate an expression.
evaluateExpression :: Parameters -- ^ The formal parameters.
                   -> Expression -- ^ The expression.
                   -> Arguments  -- ^ The argument.
                   -> Number     -- ^ The result.
evaluateExpression parameters (Power  x y) = \z -> ((**) `on` flip (evaluateExpression parameters) z) x y
evaluateExpression parameters (Times  x y) = \z -> ((*)  `on` flip (evaluateExpression parameters) z) x y
evaluateExpression parameters (Divide x y) = \z -> ((/)  `on` flip (evaluateExpression parameters) z) x y
evaluateExpression parameters (Plus   x y) = \z -> ((+)  `on` flip (evaluateExpression parameters) z) x y
evaluateExpression parameters (Minus  x y) = \z -> ((-)  `on` flip (evaluateExpression parameters) z) x y
evaluateExpression parameters (Negate   x) = negate . evaluateExpression parameters x
evaluateExpression parameters (Sin      x) = sin    . evaluateExpression parameters x
evaluateExpression parameters (Cos      x) = cos    . evaluateExpression parameters x
evaluateExpression parameters (Sqrt     x) = sqrt   . evaluateExpression parameters x
evaluateExpression parameters (Exp      x) = exp    . evaluateExpression parameters x
evaluateExpression parameters (Cis      x) = cis    . evaluateExpression parameters x
                                               where cis z = cos z + (0 :+ 1) * sin z
evaluateExpression _          (Number   x) = const x
evaluateExpression parameters (Variable x) = \z ->
                                               maybe
                                                 (error $ "Undefined variable \"" ++ x ++ "\".")
                                                 (z V.!)
                                                 $ x `V.elemIndex` parameters
