{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

-- |
-- Module      : Data.Text.Buildable
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@mailrank.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types that can be rendered to a 'Builder'.

module Data.Text.Buildable
    (
      Buildable(..)
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Text.Format.Functions ((<>))
import Data.Text.Format.Int (decimal, hexadecimal)
import Data.Text.Format.RealFloat (formatRealFloat, showFloat)
import Data.Text.Format.RealFloat.Fast (DispFloat, formatFloat, fshowFloat)
import Data.Text.Format.Types (Fast(..), Hex(..), Shown(..))
import Data.Text.Format.Types.Internal (FPControl(..))
import Data.Text.Lazy.Builder
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Clock (DiffTime, NominalDiffTime, UTCTime, UniversalTime)
import Data.Time.Clock (getModJulianDate)
import Data.Time.LocalTime (LocalTime, TimeOfDay, TimeZone, ZonedTime)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.Ptr (IntPtr, WordPtr, Ptr, ptrToWordPtr)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT

-- | The class of types that can be rendered to a 'Builder'.
class Buildable p where
    build :: p -> Builder

instance Buildable Builder where
    build = id

instance Buildable LT.Text where
    build = fromLazyText
    {-# INLINE build #-}

instance Buildable ST.Text where
    build = fromText
    {-# INLINE build #-}

instance Buildable Char where
    build = singleton
    {-# INLINE build #-}

instance Buildable [Char] where
    build = fromString
    {-# INLINE build #-}

instance (Integral a) => Buildable (Hex a) where
    build = hexadecimal
    {-# INLINE build #-}

instance Buildable Int8 where
    build = decimal
    {-# INLINE build #-}

instance Buildable Int16 where
    build = decimal
    {-# INLINE build #-}

instance Buildable Int32 where
    build = decimal
    {-# INLINE build #-}

instance Buildable Int where
    build = decimal
    {-# INLINE build #-}

instance Buildable Int64 where
    build = decimal
    {-# INLINE build #-}

instance Buildable Integer where
    build = decimal
    {-# INLINE build #-}

instance Buildable Word8 where
    build = decimal
    {-# INLINE build #-}

instance Buildable Word16 where
    build = decimal
    {-# INLINE build #-}

instance Buildable Word32 where
    build = decimal
    {-# INLINE build #-}

instance Buildable Word where
    build = decimal
    {-# INLINE build #-}

instance Buildable Word64 where
    build = decimal
    {-# INLINE build #-}

instance (Integral a, Buildable a) => Buildable (Ratio a) where
    {-# SPECIALIZE instance Buildable (Ratio Integer) #-}
    build a = build (numerator a) <> singleton '/' <> build (denominator a)

instance Buildable Float where
    build = showFloat
    {-# INLINE build #-}

instance Buildable Double where
    build = showFloat
    {-# INLINE build #-}

instance (RealFloat a) => Buildable (FPControl a) where
    build (FPControl fmt decs x) = formatRealFloat fmt decs x
    {-# INLINE build #-}

instance (RealFloat a, DispFloat a) => Buildable (Fast a) where
    build = fshowFloat . fromFast
    {-# INLINE build #-}

instance (RealFloat a, DispFloat a) => Buildable (Fast (FPControl a)) where
    build (Fast (FPControl fmt decs x)) = formatFloat fmt decs x
    {-# INLINE build #-}

instance Buildable DiffTime where
    build = build . Shown
    {-# INLINE build #-}

instance Buildable NominalDiffTime where
    build = build . Shown
    {-# INLINE build #-}

instance Buildable UTCTime where
    build = build . Shown
    {-# INLINE build #-}

instance Buildable UniversalTime where
    build = build . Shown . getModJulianDate
    {-# INLINE build #-}

instance Buildable Day where
    build = fromString . showGregorian
    {-# INLINE build #-}

instance (Show a) => Buildable (Shown a) where
    build = fromString . show . shown
    {-# INLINE build #-}

instance Buildable TimeOfDay where
    build = build . Shown
    {-# INLINE build #-}

instance Buildable TimeZone where
    build = build . Shown
    {-# INLINE build #-}

instance Buildable LocalTime where
    build = build . Shown
    {-# INLINE build #-}

instance Buildable ZonedTime where
    build = build . Shown
    {-# INLINE build #-}

instance Buildable IntPtr where
    build p = fromText "0x" <> hexadecimal p

instance Buildable WordPtr where
    build p = fromText "0x" <> hexadecimal p

instance Buildable (Ptr a) where
    build = build . ptrToWordPtr
