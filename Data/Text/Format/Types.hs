{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Data.Text.Format.Types
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@mailrank.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for text mangling.

module Data.Text.Format.Types
    (
      Format
    , Fast(..)
    , Only(..)
    , Shown(..)
    ) where

import Data.Text.Format.Types.Internal
