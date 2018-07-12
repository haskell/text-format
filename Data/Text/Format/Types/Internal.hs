{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Data.Text.Format.Types.Internal
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for text mangling.

module Data.Text.Format.Types.Internal
    (
      Format(..)
    , Only(..)
    , Shown(..)
    -- * Integer format control
    , Hex(..)
    ) where

import           Data.Monoid   (Monoid (..))
import           Data.String   (IsString (..))
import           Data.Text     (Text)
import           Data.Typeable (Typeable)

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
newtype Format = Format { fromFormat :: Text }
    deriving (Eq, Ord, Typeable, Show)

instance Semigroup Format where
    Format a <> Format b = Format (a <> b)

instance Monoid Format where
    mempty = Format mempty

instance IsString Format where
    fromString = Format . fromString

-- | Render an integral type in hexadecimal.
newtype Hex a = Hex a
    deriving (Eq, Ord, Read, Show, Num, Real, Enum, Integral)

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
