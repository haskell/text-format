-- |
-- Module      : Data.Text.Format.Params
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@mailrank.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types that can be used as a collection of arguments for formatting.

module Data.Text.Format.Params
    (
      Params(..)
    ) where

import Data.Text.Buildable
import Data.Text.Format.Types
import Data.Text.Lazy.Builder

-- | The class of types that can be used as a collection of arguments
-- for formatting.
class Params ps where
    buildParams :: ps -> [Builder]

instance (Buildable a) => Params (Only a) where
    buildParams (Only a) = [build a]

instance (Buildable a) => Params [a] where
    buildParams = map build

instance (Buildable a, Buildable b) => Params (a,b) where
    buildParams (a,b) = [build a, build b]

instance (Buildable a, Buildable b, Buildable c) => Params (a,b,c) where
    buildParams (a,b,c) = [build a, build b, build c]

instance (Buildable a, Buildable b, Buildable c, Buildable d)
    => Params (a,b,c,d) where
    buildParams (a,b,c,d) =
        [build a, build b, build c, build d]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e)
    => Params (a,b,c,d,e) where
    buildParams (a,b,c,d,e) =
        [build a, build b, build c, build d, build e]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f)
    => Params (a,b,c,d,e,f) where
    buildParams (a,b,c,d,e,f) =
        [build a, build b, build c, build d, build e,
         build f]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g)
    => Params (a,b,c,d,e,f,g) where
    buildParams (a,b,c,d,e,f,g) =
        [build a, build b, build c, build d, build e,
         build f, build g]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h)
    => Params (a,b,c,d,e,f,g,h) where
    buildParams (a,b,c,d,e,f,g,h) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i)
    => Params (a,b,c,d,e,f,g,h,i) where
    buildParams (a,b,c,d,e,f,g,h,i) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j)
    => Params (a,b,c,d,e,f,g,h,i,j) where
    buildParams (a,b,c,d,e,f,g,h,i,j) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j]
