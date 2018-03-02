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
-- | Integer functions.
--
-----------------------------------------------------------------------------


{-# LANGUAGE Safe #-}


module Data.Int.Util (
-- * Functions
  ilog2
, isqrt
, ilog2sqrt
) where


import Control.Arrow ((&&&))


-- | Square root of an integer.
--
-- This is equivalent to:
--
-- @
--   isqrt x == floor (sqrt $ from Integral x)
-- @
isqrt :: (Enum a, Num a, Ord a)
      => a -- ^ The integer.
      -> a -- ^ The integral square root.
isqrt n
  | n < fst (head table) = error "isqrt: illegal argument."
  | otherwise            = snd . head $ dropWhile ((< n) . fst) table
    where
      table = tabulate (^ (2 :: Int))


-- | Base-two logarithm of an integer.
--
-- This is equivalent to:
--
-- @
--   ilog2 x == floor (logBase 2 $ fromIntegral x)
-- @
ilog2 :: (Enum a, Integral a, Ord a)
      => a -- ^ The integer.
      -> a -- ^ The integral base-two logarithm.
ilog2 n
  | n < fst (head table) = error "ilog2: illegal argument."
  | otherwise            = snd . head $ dropWhile ((< n) . fst) table
    where
      table = tabulate (2^)


-- | Base-two logarithm of the square root of an integer.
--
-- This is equivalent to:
--
-- @
--   ilog2sqrt x == floor (logBase 2 . sqrt $ fromIntegral x)
-- @
ilog2sqrt :: (Enum a, Integral a, Num a, Ord a)
          => a -- ^ The integer.
          -> a -- ^ The integral base-two logarithm of the square root.
ilog2sqrt n
  | n < fst (head table) = error "ilog2sqrt: illegal argument."
  | otherwise            = snd . head $ dropWhile ((< n) . fst) table
    where
      table = tabulate (^ (2 :: Int))


-- | Tabulate a function for later use.
tabulate :: (Enum a, Num a)
         => (a -> a) -- ^ The function.
         -> [(a, a)] -- ^ Its tabulation.
tabulate f = (f &&& id) <$> [0..]
