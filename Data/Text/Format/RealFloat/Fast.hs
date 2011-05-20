{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Data.Text.Format.RealFloat.Fast
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     BSD3
-- Maintainer:  Daniel Fischer
-- Stability:   experimental
-- Portability: non-portable (GHC extensions)
--
-- Fast 'Builder' representations for floating point types.  The code
-- is largely taken from code in "GHC.Float" and the 'Show' instance
-- of 'Integer' in "GHC.Num" to get the sequence of digits.
module Data.Text.Format.RealFloat.Fast
    ( DispFloat(..)
    , fshowFloat
    , fshowEFloat
    , fshowFFloat
    , fshowGFloat
    ) where

import Data.Text.Format.Functions ((<>), i2d)
import Data.Text.Format.Int (integral)
import Data.Text.Format.RealFloat.Fast.Internal (posToDigits)
import Data.Text.Format.RealFloat.Functions (roundTo)
import Data.Text.Format.Types (Format(..))
import Data.Text.Lazy.Builder
import qualified Data.Text as T

-- | Class for specifying display parameters. The type @a@
--   is supposed to be an IEEE-ish (real) floating-point
--   type with floating-point radix 2, such that the mantissa
--   returned by 'decodeFloat' satisfies
--
-- @
--   2^('binExp' x) <= 'fst' ('decodeFloat' x) < 2^('binExp' x + 1)
-- @
--
--   for @x > 0@, so @'binExp' x = 'floatDigits' x - 1@.
--   The number of decimal digits that may be required is calculated
--   with the formula
--
-- @
--   'decDigits' x = 2 + 'floor' ('floatDigits' x * 'logBase' 10 2).
-- @
--
--   The default implementation uses an approximation of
--   @'logBase' 10 2@ sufficient for mantissae of up to
--   several thousand bits. Nevertheless, hardcoding
--   the values in instance declarations may yield
--   better performance.
class (RealFloat a) => DispFloat a where
  -- | The number of decimal digits that may be needed to
  --   uniquely determine a value of type @a@.
  --   For faster conversions which need not satisfy
  --
  -- @
  --   x == 'read' ('fshow' x)
  -- @
  --
  --   a smaller value can be given.
  decDigits     :: a -> Int
  decDigits x   = 2 + (8651*(floatDigits x)) `quot` 28738
  -- | The base 2 logarithm of the mantissa returned by
  --   @'decodeFloat' x@ for @x > 0@.
  binExp        :: a -> Int
  binExp x      = floatDigits x - 1

instance DispFloat Double where
  decDigits _   = 17
  binExp _      = 52

instance DispFloat Float where
  decDigits _   = 9
  binExp _      = 23

-- | Show a signed 'DispFloat' value to full precision
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
-- Analogous to @'showFloat'@ from "GHC.Float".
fshowFloat :: (DispFloat a) => a -> Builder
{-# SPECIALIZE fshowFloat :: Float -> Builder #-}
{-# SPECIALIZE fshowFloat :: Double -> Builder #-}
fshowFloat x = formatFloat Generic Nothing x

-- | Show a signed 'DispFloat' value
-- using scientific (exponential) notation (e.g. @2.45e2@, @1.5e-3@).
--
-- In the call @'fshowEFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then @'max' 1 d@ digits after the decimal point are shown.
-- Analogous to @'showEFloat'@ from "Numeric".
fshowEFloat    :: (DispFloat a) => Maybe Int -> a -> Builder
{-# SPECIALIZE fshowEFloat :: Maybe Int -> Float -> Builder #-}
{-# SPECIALIZE fshowEFloat :: Maybe Int -> Double -> Builder #-}
fshowEFloat d x =  formatFloat Exponent d x

-- | Show a signed 'DispFloat' value
-- using standard decimal notation (e.g. @245000@, @0.0015@).
--
-- In the call @'fshowFFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then @'max' 0 d@ digits after the decimal point are shown.
-- Analogous to @'showFFloat'@ from "Numeric".
fshowFFloat    :: (DispFloat a) => Maybe Int -> a -> Builder
{-# SPECIALIZE fshowFFloat :: Maybe Int -> Float -> Builder #-}
{-# SPECIALIZE fshowFFloat :: Maybe Int -> Double -> Builder #-}
fshowFFloat d x =  formatFloat Fixed d x

-- | Show a signed 'DispFloat' value
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
-- In the call @'fshowGFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then @'max' 1 d@ digits after the decimal point are shown.
-- Analogous to @'showGFloat'@ from "Numeric".
fshowGFloat    :: (DispFloat a) => Maybe Int -> a -> Builder
{-# SPECIALIZE fshowGFloat :: Maybe Int -> Float -> Builder #-}
{-# SPECIALIZE fshowGFloat :: Maybe Int -> Double -> Builder #-}
fshowGFloat d x =  formatFloat Generic d x

formatFloat :: DispFloat a => Format -> Maybe Int -> a -> Builder
{-# SPECIALIZE formatFloat :: Format -> Maybe Int -> Float -> Builder #-}
{-# SPECIALIZE formatFloat :: Format -> Maybe Int -> Double -> Builder #-}
formatFloat fmt decs x
    | isNaN x                   = "NaN"
    | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
    | x < 0 || isNegativeZero x = singleton '-' <> doFmt fmt (fltDigs (-x))
    | otherwise                 = doFmt fmt (fltDigs x)
      where
        fltDigs 0 = ([0],0)
        fltDigs y = uncurry (posToDigits (decDigits y) (binExp y)) (decodeFloat y)
        fluff :: [Int] -> [Int]
        fluff [] = [0]
        fluff xs = xs

        doFmt format (is, e) =
          case format of
            Generic ->
              doFmt (if e < 0 || e > 7 then Exponent else Fixed) (is,e)
            Exponent ->
              case decs of
                Nothing ->
                  let show_e' = integral $ if ei == 0 then (e-1) else e
                      (ei,(d:ds)) = roundToS (decDigits x) is
                  in case is of
                       [0] -> "0.0e0"
                       _ -> singleton (i2d d) <> singleton '.' <> fromString (map i2d (fluff ds)) <> singleton 'e' <> show_e'
                Just dec ->
                  let dec' = max dec 1 in
                  case is of
                    [0] -> fromText "0." <> fromText (T.replicate dec' "0") <> "e0"
                    _ -> let (ei,is') = roundTo (dec'+1) is
                             (d:ds') = map i2d (if ei > 0 then init is' else is')
                         in singleton d <> singleton '.' <> fromString ds' <> singleton 'e' <> integral (e-1+ei)
            Fixed ->
              let mk0 ls = case ls of { "" -> "0" ; _ -> fromString ls} in
              case decs of
                Nothing ->
                  let (ei, is') = roundToS (decDigits x) is
                      e' = e+ei
                      ds = map i2d is'
                  in case is of
                       [0] -> "0.0"
                       _ | e' <= 0 -> "0." <> fromText (T.replicate (-e') "0") <> fromString (map i2d is')
                         | otherwise ->
                           let f 0 s    rs  = mk0 (reverse s) <> singleton '.' <> mk0 rs
                               f n s    ""  = f (n-1) ('0':s) ""
                               f n s (r:rs) = f (n-1) (r:s) rs
                           in f e' "" ds
                Just dec ->
                  let dec' = max dec 0 in
                  if e >= 0 then
                     let (ei,is') = roundTo (dec' + e) is
                         (ls,rs)  = splitAt (e+ei) (map i2d is')
                     in mk0 ls <> (if null rs then "" else singleton '.' <> fromString rs)
                  else
                     let (ei,is') = roundTo dec' (replicate (-e) 0 ++ is)
                         d:ds' = map i2d (if ei > 0 then is' else 0:is')
                     in singleton d <> (if null ds' then "" else singleton '.' <> fromString ds')

roundToS :: Int -> [Int] -> (Int,[Int])
roundToS d is =
    case f d is of
      x@(0,_) -> x
      (1,xs)  -> (1, 1:xs)
      _       -> error "roundToS: bad Value"
  where
    f _ []          = (0, [])
    f 0 (x:_)       = (if x >= 5 then 1 else 0, [])
    f n (i:xs)
      | i' == 10    = (1,prep 0 ds)
      | otherwise   = (0,prep i' ds)
        where
          prep 0 [] = []
          prep a bs = a:bs
          (c,ds)    = f (n-1) xs
          i'        = c + i
