{-# LANGUAGE CPP, BangPatterns, MagicHash, UnboxedTuples #-}
-- |
-- Module:          Data.Text.Format.RealFloat.Fast.Internal
-- Copyright:       (c) 2011 Daniel Fischer
-- Licence:         BSD3
-- Maintainer:      Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:       experimental
-- Portability:     non-portable (GHC extensions)
--
-- Faster digit string generation for floating point numbers.
-- Uses a modification of the Integer showing code from "GHC.Num".
module Data.Text.Format.RealFloat.Fast.Internal
    (
      posToDigits
    ) where

#include "MachDeps.h"

import Data.Array.Base (unsafeAt)
import Data.Array.IArray
import GHC.Base
import GHC.Integer
import GHC.Num (quotRemInt)

#if WORD_SIZE_IN_BITS == 32
#define DIGITS       9
#define BASE         1000000000
#else
#define DIGITS       18
#define BASE         1000000000000000000
#endif

-- digits and exponent for a floating point number.
-- floatRadix is assumed to be 2, decodeFloat to return
-- a mantissa 2^(floatDigits-1) <= mantissa < 2^floatDigits
posToDigits :: Int -> Int -> Integer -> Int -> ([Int],Int)
posToDigits showDigs mantExp mant scaleExp@(I# e#) = (integerToDigits decMant, e10)
  where
    !rex = mantExp + scaleExp
    !l0 = (8651*rex) `quot` 28738
    !l10 = if rex < 0 then l0-1 else l0
    -- 10^l10 <= x < 10^(l10+2)
    !decshift@(I# d#) = showDigs - l10
    !binshift = e# +# d#
    !decMant
        | d# <# 0# =
            (if binshift <# 0#
                then shiftRInteger mant (negateInt# binshift)
                else shiftLInteger mant binshift) `quot` expt5 (I# (negateInt# d#))
        | binshift <# 0# =
            shiftRInteger (mant * expt5 decshift) (negateInt# binshift)
        | otherwise = shiftLInteger (mant * expt5 decshift) binshift
    !e10 = if decMant < expt10 (showDigs+1) then l10+1 else l10+2

expt5 :: Int -> Integer
expt5 k = if k <= maxEx5 && k >= 0 then unsafeAt expts5 k else 5^k

expt10 :: Int -> Integer
expt10 k = if k <= maxEx10 && k >= 0 then unsafeAt expts10 k else 10^k

maxEx5 :: Int
maxEx5 = 349

maxEx10 :: Int
maxEx10 = 25

expts5 :: Array Int Integer
expts5 = array (0, maxEx5) [(k,5^k) | k <- [0 .. maxEx5]]

expts10 :: Array Int Integer
expts10 = array (0,maxEx10) [(k,10^k) | k <- [0 .. maxEx10]]

------------------------------------------------------------------------------
--  The code to show Integers, modified to produce [Int] instead of [Char]
--  Taken from GHC.Num and modified to suit our needs
--  The GHC Licence is reproduced in the package root

-- Divide and conquer implementation
-- generate the sequence of digits of a positive Integer
integerToDigits :: Integer -> [Int]
integerToDigits nm = integerToDigits' nm []

integerToDigits' :: Integer -> [Int] -> [Int]
integerToDigits' nm ds
    | nm < BASE = jhead (fromInteger nm) ds
    | otherwise = case nm `quotRemInteger` BASE of
                    (# q, r #) -> integerToDigits' q (jblock (fromInteger r) ds)
      where
        -- Convert an integer that fits into a machine word. Again, we have two
        -- functions, one that drops leading zeros (jhead) and one that doesn't
        -- (jblock)
        jhead :: Int -> [Int] -> [Int]
        jhead n cs
            | n < 10    = n:cs
            | otherwise = jhead q (r : cs)
            where
            (q, r) = n `quotRemInt` 10

        jblock = jblock' {- ' -} DIGITS     -- bloody CPP

        jblock' :: Int -> Int -> [Int] -> [Int]
        jblock' d n cs
            | d == 1    = n : cs
            | otherwise = jblock' (d - 1) q (r : cs)
            where
            (q, r) = n `quotRemInt` 10
