module Data.Text.Format.Params
    (
      Params(..)
    ) where

import Data.Text.Format.Param
import Data.Text.Format.Types
import Data.Text.Lazy.Builder

class Params ps where
    buildParams :: ps -> [Builder]

instance (Param a) => Params (Only a) where
    buildParams (Only a) = [buildParam a]

instance (Param a) => Params [a] where
    buildParams = map buildParam

instance (Param a, Param b) => Params (a,b) where
    buildParams (a,b) = [buildParam a, buildParam b]
