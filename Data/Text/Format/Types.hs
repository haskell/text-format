{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Data.Text.Format.Types
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@mailrank.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for text mangling.

module Data.Text.Format.Types
    (
      FPFormat(..)
    , Fast(..)
    , Only(..)
    , Shown(..)
    ) where

-- | Control the rendering of floating point numbers.
data FPFormat = Exponent
              -- ^ Scientific notation (e.g. @2.3e123@).
              | Fixed
              -- ^ Standard decimal notation.
              | Generic
              -- ^ Use decimal notation for values between @0.1@ and
              -- @9,999,999@, and scientific notation otherwise.
                deriving (Enum, Read, Show)

-- | Render a floating point number using a much faster algorithm than
-- the default (up to 10x faster). This performance comes with a
-- potential cost in readability, as the faster algorithm can produce
-- strings that are longer than the default algorithm
-- (e.g. \"@1.3300000000000001@\" instead of \"@1.33@\").
newtype Fast a = Fast {
      fromFast :: a
    } deriving (Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac,
                Floating, RealFloat)

-- | Use this @newtype@ wrapper for your single parameter if you are
-- formatting a string containing exactly one substitution site.
newtype Only a = Only {
      fromOnly :: a
    } deriving (Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac,
                Floating, RealFloat, Enum, Integral, Bounded)

-- | Render a value using its 'Show' instance.
newtype Shown a = Shown {
      shown :: a
    } deriving (Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac,
                Floating, RealFloat, Enum, Integral, Bounded)
