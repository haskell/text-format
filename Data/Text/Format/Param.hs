{-# LANGUAGE FlexibleInstances #-}

module Data.Text.Format.Param
    (
      Param(..)
    ) where

import Data.Text.Lazy.Builder
import Data.Text.Format.Int
import qualified Data.Text.Lazy as LT
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Text as ST

class Param p where
    buildParam :: p -> Builder

instance Param LT.Text where
    buildParam = fromLazyText

instance Param ST.Text where
    buildParam = fromText

instance Param Char where
    buildParam = singleton

instance Param [Char] where
    buildParam = fromText . ST.pack

instance Param Int8 where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Int16 where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Int32 where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Int where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Int64 where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Integer where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Word8 where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Word16 where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Word32 where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Word where
    buildParam = integral
    {-# INLINE buildParam #-}

instance Param Word64 where
    buildParam = integral
    {-# INLINE buildParam #-}
