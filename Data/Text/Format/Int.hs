{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}

-- Module:      Blaze.Text.Int
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize an integral value as a lazy 'L.ByteString'.

module Data.Text.Format.Int
    (
      digit
    , integral
    , minus
    ) where

import Data.Char (chr)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (mappend, mempty)
import Data.Text.Lazy.Builder
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Base (quotInt, remInt)
import GHC.Num (quotRemInteger)
import GHC.Types (Int(..))

#ifdef  __GLASGOW_HASKELL__
# if __GLASGOW_HASKELL__ < 611
import GHC.Integer.Internals
# else
import GHC.Integer.GMP.Internals
# endif
#endif

#ifdef INTEGER_GMP
# define PAIR(a,b) (# a,b #)
#else
# define PAIR(a,b) (a,b)
#endif

integral :: Integral a => a -> Builder
{-# SPECIALIZE integral :: Int -> Builder #-}
{-# SPECIALIZE integral :: Int8 -> Builder #-}
{-# SPECIALIZE integral :: Int16 -> Builder #-}
{-# SPECIALIZE integral :: Int32 -> Builder #-}
{-# SPECIALIZE integral :: Int64 -> Builder #-}
{-# SPECIALIZE integral :: Word -> Builder #-}
{-# SPECIALIZE integral :: Word8 -> Builder #-}
{-# SPECIALIZE integral :: Word16 -> Builder #-}
{-# SPECIALIZE integral :: Word32 -> Builder #-}
{-# SPECIALIZE integral :: Word64 -> Builder #-}
{-# RULES "integral/Integer" integral = integer :: Integer -> Builder #-}
integral i
    | i < 0     = minus `mappend` go (-i)
    | otherwise = go i
  where
    go n | n < 10    = digit n
         | otherwise = go (n `quot` 10) `mappend` digit (n `rem` 10)

digit :: Integral a => a -> Builder
digit n = singleton $! chr (fromIntegral n + 48)
{-# INLINE digit #-}

minus :: Builder
minus = singleton '-'

int :: Int -> Builder
int = integral
{-# INLINE int #-}

integer :: Integer -> Builder
integer (S# i#) = int (I# i#)
integer i
    | i < 0     = minus `mappend` go (-i)
    | otherwise = go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf p n
      | p > n       = [n]
      | otherwise   = splith p (splitf (p*p) n)

    splith p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) | q > 0     -> q : r : splitb p ns
                                  | otherwise -> r : splitb p ns
    splith _ _      = error "splith: the impossible happened."

    splitb p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) -> q : r : splitb p ns
    splitb _ _      = []

data T = T !Integer !Int

fstT :: T -> Integer
fstT (T a _) = a

maxInt :: Integer
maxDigits :: Int
T maxInt maxDigits =
    until ((>mi) . (*10) . fstT) (\(T n d) -> T (n*10) (d+1)) (T 10 1)
  where mi = fromIntegral (maxBound :: Int)

putH :: [Integer] -> Builder
putH (n:ns) = case n `quotRemInteger` maxInt of
                PAIR(x,y)
                    | q > 0     -> int q `mappend` pblock r `mappend` putB ns
                    | otherwise -> int r `mappend` putB ns
                    where q = fromInteger x
                          r = fromInteger y
putH _ = error "putH: the impossible happened"

putB :: [Integer] -> Builder
putB (n:ns) = case n `quotRemInteger` maxInt of
                PAIR(x,y) -> pblock q `mappend` pblock r `mappend` putB ns
                    where q = fromInteger x
                          r = fromInteger y
putB _ = mempty

pblock :: Int -> Builder
pblock = go maxDigits
  where
    go !d !n
        | d == 1    = digit n
        | otherwise = go (d-1) q `mappend` digit r
        where q = n `quotInt` 10
              r = n `remInt` 10
