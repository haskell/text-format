{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.Text.Format
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Text.Printf as P

printf1 :: (P.PrintfArg a) => String -> a -> String
printf1 f a = P.printf f a

printf2 :: (P.PrintfArg a, P.PrintfArg b) => String -> (a,b) -> String
printf2 f (a,b) = P.printf f a b

printf3 :: (P.PrintfArg a, P.PrintfArg b, P.PrintfArg c) =>
           String -> (a,b,c) -> String
printf3 f (a,b,c) = P.printf f a b c

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
       , bgroup "comparison" [
           bench "format1" $ nf (format "hi mom {}\n") (Only (pi::Double))
         , bench "printf1" $ nf (printf1 "hi mom %f\n") (pi::Double)
         , bench "show1" $ nf (\d -> "hi mom " ++ show d ++ "\n") (pi::Double)
         , bench "format2" $ nf (format "hi mom {} {}\n") (pi::Double, "yeah"::T.Text)
         , bench "printf2" $ nf (printf2 "hi mom %f %s\n") (pi::Double, "yeah"::String)
         , bench "show2" $ nf (\(d,s) -> "hi mom " ++ show d ++ " " ++ show s ++ "\n") (pi::Double, "yeah"::String)
         , bench "format3" $ nf (format "hi mom {} {} {}\n") (pi::Double, "yeah"::T.Text, 21212121::Int)
         , bench "printf3" $ nf (printf3 "hi mom %f %s %d\n") (pi::Double, "yeah"::String, 21212121::Int)
         ]
       , bgroup "types" [
           bench "unit" $ nf (format "hi") ()
         , bgroup "int" [
             bench "small" $ nf (format "hi {}") (Only (1::Int))
           , bench "medium" $ nf (format "hi {}") (Only (1234::Int))
           , bench "large" $ nf (format "hi {}") (Only (0x7fffffff::Int))
           ]
         , bgroup "float" [
             bench "small" $ nf (format "hi {}") (Only (1::Float))
           , bench "medium" $ nf (format "hi {}") (Only (pi::Float))
           , bench "large" $ nf (format "hi {}") (Only (pi*1e37::Float))
           ]
         , bgroup "double" [
             bench "small" $ nf (format "hi {}") (Only (1::Double))
           , bench "medium" $ nf (format "hi {}") (Only (pi::Double))
           , bench "large" $ nf (format "hi {}") (Only (pi*1e37::Double))
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
