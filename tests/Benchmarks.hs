{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.Text.Format
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

main = defaultMain [
         bgroup "arity" [
           bench "0" $ nf (format "hi") ()
         , bench "1" $ nf (format "hi {}") (Only $ T.pack "mom")
         , bench "2" $ nf (format "hi {}, how are {}")
                       (T.pack "mom", T.pack "you")
         , bench "3" $ nf (format "hi {}, how are {} keeping {}")
                       (T.pack "mom", T.pack "you", T.pack "now")
         , bench "4" $ nf (format "hi {}, {} - how are {} keeping {}")
                       (T.pack "mom", T.pack "hey", T.pack "you", T.pack "now")
         ]
       , bgroup "types" [
           bench "unit" $ nf (format "hi") ()
         , bgroup "int" [
             bench "small" $ nf (format "hi {}") (Only (1::Int))
           , bench "medium" $ nf (format "hi {}") (Only (1234::Int))
           , bench "large" $ nf (format "hi {}") (Only (0x7fffffff::Int))
           ]
         , bgroup "float" [
             bgroup "slow" [
               bench "small" $ nf (format "hi {}") (Only (1::Float))
             , bench "medium" $ nf (format "hi {}") (Only (pi::Float))
             , bench "large" $ nf (format "hi {}") (Only (pi*1e37::Float))
             ]
           , bgroup "fast" [
               bench "small" $ nf (format "hi {}") (Only (1::Fast Float))
             , bench "medium" $ nf (format "hi {}") (Only (pi::Fast Float))
             , bench "large" $ nf (format "hi {}") (Only (pi*1e37::Fast Float))
             ]
           ]
         , bgroup "double" [
             bgroup "slow" [
               bench "small" $ nf (format "hi {}") (Only (1::Double))
             , bench "medium" $ nf (format "hi {}") (Only (pi::Double))
             , bench "large" $ nf (format "hi {}") (Only (pi*1e37::Double))
             ]
           , bgroup "fast" [
               bench "small" $ nf (format "hi {}") (Only (1::Fast Double))
             , bench "medium" $ nf (format "hi {}") (Only (pi::Fast Double))
             , bench "large" $ nf (format "hi {}") (Only (pi*1e37::Fast Double))
             ]
           ]
         , bgroup "string" [
             bench "small" $ nf (format "hi {}") (Only ("mom" :: String))
           , bench "medium" $ nf (format "hi {}")
                              (Only . concat . replicate 64 $ ("mom" :: String))
           , bench "large" $ nf (format "hi {}")
                             (Only . concat . replicate 1024 $ ("mom" :: String))
           ]
         , bgroup "text" [
             bench "small" $ nf (format "hi {}") (Only (T.pack "mom"))
           , bench "medium" $ nf (format "hi {}") (Only (T.replicate 64 "mom"))
           , bench "large" $ nf (format "hi {}") (Only (T.replicate 1024 "mom"))
           ]
           , bgroup "lazytext" [
               bench "small" $ nf (format "hi {}") (Only (L.pack "mom"))
             , bench "medium" $ nf (format "hi {}")
                                (Only . L.fromChunks . replicate 64 $ "mom")
             , bench "large" $ nf (format "hi {}")
                               (Only . L.fromChunks . replicate 1024 $ "mom")
             ]
           ]
         ]
