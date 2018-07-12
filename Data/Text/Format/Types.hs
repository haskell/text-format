{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Data.Text.Format.Types
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for text mangling.

module Data.Text.Format.Types
    (
      Format
    , Only(..)
    , Shown(..)
    -- * Integer format control
    , Hex(..)
    ) where

import           Data.Text.Format.Types.Internal
