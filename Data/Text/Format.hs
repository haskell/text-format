{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Format
    where

import Data.Monoid
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import Data.Text.Lazy.Builder
import Data.Text.Format.Param
import Data.Text.Format.Params

build :: Params ps => ST.Text -> ps -> Builder
build fmt ps
    | null xs && not ("{}" `ST.isInfixOf` fmt) = fromText fmt
    | otherwise = zipParams (map fromText . ST.splitOn "{}" $ fmt) xs
  where xs = buildParams ps
        zipParams (f:fs) (y:ys) = f `mappend` y `mappend` zipParams fs ys
        zipParams [f] [] = f
        zipParams _ _ = error "oops"

format :: Params ps => ST.Text -> ps -> LT.Text
format fmt ps = toLazyText $ build fmt ps
