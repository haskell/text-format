{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Format
    (
      Fast(..)
    , Only(..)
    , format
    , build
    , print
    , hprint
    , left
    , right
    ) where

import qualified Data.Text.Buildable as B
import Data.Text.Format.Params (Params(..))
import Data.Text.Format.Functions ((<>))
import Data.Text.Format.Types (Fast(..), Only(..))
import Data.Text.Lazy.Builder
import Prelude hiding (print)
import System.IO (Handle)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

build :: Params ps => ST.Text -> ps -> Builder
build fmt ps = zipParams (map fromText . ST.splitOn "{}" $ fmt) xs
  where zipParams (f:fs) (y:ys) = f <> y <> zipParams fs ys
        zipParams [f] []        = f
        zipParams _ _ = error . LT.unpack $ format
                        "Data.Text.Format.build: {} sites, but {} parameters"
                        (ST.count "{}" fmt, length xs)
        xs = buildParams ps

format :: Params ps => ST.Text -> ps -> LT.Text
format fmt ps = toLazyText $ build fmt ps

print :: Params ps => ST.Text -> ps -> IO ()
print fmt ps = LT.putStr . toLazyText $ build fmt ps

hprint :: Params ps => Handle -> ST.Text -> ps -> IO ()
hprint h fmt ps = LT.hPutStr h . toLazyText $ build fmt ps

left :: B.Buildable a => Int -> Char -> a -> Builder
left k c =
    fromLazyText . LT.justifyLeft (fromIntegral k) c . toLazyText . B.build

right :: B.Buildable a => Int -> Char -> a -> Builder
right k c =
    fromLazyText . LT.justifyRight (fromIntegral k) c . toLazyText . B.build
