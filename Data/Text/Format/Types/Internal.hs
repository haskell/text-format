{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Data.Text.Format.Types.Internal
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@mailrank.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for text mangling.

module Data.Text.Format.Types.Internal
    (
      Format(..)
    , Only(..)
    , Shown(..)
    -- * Floating point format control
    , Fast(..)
    , FPControl(..)
    , FPFormat(..)
    ) where

import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Typeable (Typeable)

-- | A format string. This is intentionally incompatible with other
-- string types, to make it difficult to construct a format string by
-- concatenating string fragments (a very common way to accidentally
-- make code vulnerable to malicious data).
--
-- This type is an instance of 'IsString', so the easiest way to
-- construct a query is to enable the @OverloadedStrings@ language
-- extension and then simply write the query in double quotes.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.Text.Format
-- >
-- > f :: Format
-- > f = "hello {}"
--
-- The underlying type is 'Text', so literal Haskell strings that
-- contain Unicode characters will be correctly handled.
newtype Format = Format Text
    deriving (Eq, Ord, Typeable)

instance Monoid Format where
    Format a `mappend` Format b = Format (a `mappend` b)
    mempty = Format mempty

instance IsString Format where
    fromString = Format . fromString

-- | Control the rendering of floating point numbers.
data FPFormat = Exponent
              -- ^ Scientific notation (e.g. @2.3e123@).
              | Fixed
              -- ^ Standard decimal notation.
              | Generic
              -- ^ Use decimal notation for values between @0.1@ and
              -- @9,999,999@, and scientific notation otherwise.
                deriving (Enum, Read, Show)

-- | A floating point number, complete with rendering instructions.
data FPControl a = FPControl FPFormat (Maybe Int) a

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
