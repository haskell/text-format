{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import Control.Monad
import System.Environment
import qualified Data.Text.Format as T
import Data.Time.Clock

main = do
  args <- getArgs
  let count = case args of
                (x:_) -> read x :: Int
                _     -> 100000
  start <- getCurrentTime
  forM_ [0..count] $ \i -> do
    let !t = T.format "hi mom {}\n" [fromIntegral i * pi::Double]
    return ()
  elapsed <- (`diffUTCTime` start) `fmap` getCurrentTime
  T.print "{} iterations in {} secs ({} thousand/sec\n"
       (count, elapsed,
        fromRational (toRational count / toRational elapsed / 1e3) :: Double)
