module Data.Text.Format.Types
    (
      Only(..)
    ) where

newtype Only a = Only a
    deriving (Eq, Ord, Read, Show)
