{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Text.Format.Types
    (
      Format(..)
    , Fast(..)
    , Only(..)
    , Shown(..)
    ) where

data Format = Exponent | Fixed | Generic

newtype Fast a = Fast {
      fromFast :: a
    } deriving (Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac,
                Floating, RealFloat)

newtype Only a = Only {
      fromOnly :: a
    } deriving (Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac,
                Floating, RealFloat, Enum, Integral, Bounded)

newtype Shown a = Shown {
      shown :: a
    } deriving (Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac,
                Floating, RealFloat, Enum, Integral, Bounded)
