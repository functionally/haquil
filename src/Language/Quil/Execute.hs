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
  CDictionary
, evaluateExpression
) where


import Data.Complex (Complex((:+)))
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Language.Quil.Types (Expression(..), Number, Variable)


-- | Lookup table for values of variables.
type CDictionary = [(Variable, Number)]


-- | Evaluate an expression.
evaluateExpression :: CDictionary -> Expression -> Number
evaluateExpression cd (Power  x y) = ((**)  `on` evaluateExpression cd) x y
evaluateExpression cd (Times  x y) = ((*)   `on` evaluateExpression cd) x y
evaluateExpression cd (Divide x y) = ((/)   `on` evaluateExpression cd) x y
evaluateExpression cd (Plus   x y) = ((+)   `on` evaluateExpression cd) x y
evaluateExpression cd (Minus  x y) = ((-)   `on` evaluateExpression cd) x y
evaluateExpression cd (Negate   x) =  negate $   evaluateExpression cd  x
evaluateExpression cd (Sin      x) =  sin    $   evaluateExpression cd  x
evaluateExpression cd (Cos      x) =  cos    $   evaluateExpression cd  x
evaluateExpression cd (Sqrt     x) =  sqrt   $   evaluateExpression cd  x
evaluateExpression cd (Exp      x) =  exp    $   evaluateExpression cd  x
evaluateExpression cd (Cis      x) =  cis    $   evaluateExpression cd  x where cis z = cos z + (0 :+ 1) * sin z
evaluateExpression _  (Number   x) = x
evaluateExpression cd (Variable x) = fromMaybe (error $ "Undefined variable \"" ++ x ++ "\".") $ lookup x cd
