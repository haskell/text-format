{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Text.Format
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@mailrank.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, efficient, flexible support for formatting text strings.

module Data.Text.Format
    (
    -- * Types
      Format
    , Only(..)
    -- ** Types for format control
    , Fast(..)
    , Shown(..)
    -- * Functions
    -- ** Rendering
    , format
    , print
    , hprint
    , build
    -- ** Functions for format control
    , left
    , right
    ) where

import qualified Data.Text.Buildable as B
import Data.Text.Format.Params (Params(..))
import Data.Text.Format.Functions ((<>))
import Data.Text.Format.Types.Internal (Fast(..), Format(..), Only(..), Shown(..))
import Data.Text.Lazy.Builder
import Prelude hiding (print)
import System.IO (Handle)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

-- | Render a format string and arguments to a 'Builder'.
build :: Params ps => Format -> ps -> Builder
build (Format fmt) ps = zipParams (map fromText . ST.splitOn "{}" $ fmt) xs
  where zipParams (f:fs) (y:ys) = f <> y <> zipParams fs ys
        zipParams [f] []        = f
        zipParams _ _ = error . LT.unpack $ format
                        "Data.Text.Format.build: {} sites, but {} parameters"
                        (ST.count "{}" fmt, length xs)
        xs = buildParams ps

-- | Render a format string and arguments to a 'LT.Text'.
format :: Params ps => Format -> ps -> LT.Text
format fmt ps = toLazyText $ build fmt ps

-- | Render a format string and arguments, then print the result.
print :: Params ps => Format -> ps -> IO ()
print fmt ps = LT.putStr . toLazyText $ build fmt ps

-- | Render a format string and arguments, then print the result to
-- the given file handle.
hprint :: Params ps => Handle -> Format -> ps -> IO ()
hprint h fmt ps = LT.hPutStr h . toLazyText $ build fmt ps

-- | Pad the left hand side of a string until it reaches @k@
-- characters wide, filling with character @c@.
left :: B.Buildable a => Int -> Char -> a -> Builder
left k c =
    fromLazyText . LT.justifyLeft (fromIntegral k) c . toLazyText . B.build

-- | Pad the right hand side of a string until it reaches @k@
-- characters wide, filling with character @c@.
right :: B.Buildable a => Int -> Char -> a -> Builder
right k c =
    fromLazyText . LT.justifyRight (fromIntegral k) c . toLazyText . B.build
