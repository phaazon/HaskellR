-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Run H on a number of R programs of increasing size and complexity, comparing
-- the output of H with the output of R.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Foreign.R as R
import Foreign.R (SEXP)
import H.Prelude as H
import Language.R.QQ
import qualified Data.Vector.SEXP as SVector
import qualified Data.Vector.SEXP.Mutable as SMVector
import Control.Memory.Region

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.Int
import Data.Singletons (sing)
import qualified Data.Text.Lazy as Text
import Test.Tasty.HUnit hiding ((@=?))

hFib :: SEXP s R.Int -> R s (SEXP s R.Int)
hFib n@(H.fromSEXP -> (0 :: Int32)) = fmap (flip R.asTypeOf n) [r| as.integer(0) |]
hFib n@(H.fromSEXP -> (1 :: Int32)) = fmap (flip R.asTypeOf n) [r| as.integer(1) |]
hFib n =
    (`R.asTypeOf` n) <$>
      [r| as.integer(hFib_hs(as.integer(n_hs - 1)) + hFib_hs(as.integer(n_hs - 2))) |]

-- | Version of '(@=?)' that works in the R monad.
(@=?) :: H.Show a => String -> a -> R s ()
expected @=? actual = liftIO $ do
    assertEqual "" expected (Text.unpack (H.show actual))

main :: IO ()
main = H.withEmbeddedR H.defaultConfig $ H.runRegion $ do

    -- Placing it before enabling gctorture2 for speed.
    ("4181L" @=?) =<< hFib =<< H.mkSEXP (19 :: Int32)

    _ <- [r| gctorture2(1,0,TRUE) |]

    ("1" @=?) =<< [r| 1 |]

    -- Should be: [1] 1
    -- H.print [rsafe| 1 |] -- XXX Fails with -O0 and --enable-strict-barrier

    ("3" @=?) =<< [r| 1 + 2 |]

    -- Should be: [1] 2
    -- H.print [rsafe| base::`+`(1, 2) |]  -- XXX Fails with -O0 and --enable-strict-barrier

    ("c(\"1\", \"2\", \"3\")" @=?) =<< [r| c(1,2,"3") |]

    ("2" @=?) =<< [r| x <- 2 |]

    ("3" @=?) =<< [r| x+1 |]

    let y = (5::Double)
    ("6" @=?) =<< [r| y_hs + 1 |]

    ("function (y = ) 5 + y" @=?) =<< [r| function(y) y_hs + y |]

    _ <- [r| z <- function(y) y_hs + y |]
    ("8" @=?) =<< [r| z(3) |]

    ("1:10" @=?) =<< [r| y <- c(1:10) |]

    let foo1 = (\x -> (return $ x+1 :: R s Double))
    let foo2 = (\x -> (return $ map (+1) x :: R s [Int32]))

    ("3" @=?) =<< [r| (function(x).Call(foo1_hs,x))(2) |]

    ("2:11" @=?) =<< [r| (function(x).Call(foo2_hs,x))(y) |]

    ("43" @=?) =<< [r| x <- 42 ; x + 1 |]

    let xs = [1,2,3]::[Double]
    ("c(1, 2, 3)" @=?) =<< [r| xs_hs |]

    ("8" @=?) =<< [r| foo1_hs(7) |]

    ("NULL" @=?) H.nilValue

    let foo3 = (\n -> fmap fromSomeSEXP [r| n_hs |]) :: Int32 -> R s Int32
    ("3L" @=?) =<< [r| foo3_hs(as.integer(3)) |]

    let foo4 = (\n m -> return $ n + m) :: Double -> Double -> R s Double
    ("99" @=?) =<< [r| foo4_hs(33, 66) |]

    let fact n = if n == (0 :: Int32) then (return 1 :: R s Int32) else fmap dynSEXP [r| as.integer(n_hs * fact_hs(as.integer(n_hs - 1))) |]
    ("120L" @=?) =<< [r| fact_hs(as.integer(5)) |]

    let foo5  = \(n :: Int32) -> return (n+1) :: R s Int32
    let apply = \(n :: R.Callback s) (m :: Int32) -> [r| .Call(n_hs, m_hs) |] :: R s (R.SomeSEXP s)
    ("29L" @=?) =<< [r| apply_hs(foo5_hs, as.integer(28) ) |]

    sym <- H.install "blah"
    ("blah" @=?) sym

    _ <- [r| `+` <- function(x,y) x * y |]
    ("100" @=?) =<< [r| 10 + 10 |]

    ("20" @=?) =<< [r| base::`+`(10,10) |]

    -- restore usual meaning of `+`
    _ <- [r| `+` <- base::`+` |]

    -- test Vector literal instance
    v1 <- do
      x <- SMVector.new 3 :: R s (SMVector.MVector V 'R.Int s Int32)
      SMVector.unsafeWrite x 0 1
      SMVector.unsafeWrite x 1 2
      SMVector.unsafeWrite x 2 3
      return x
    ("c(7, 2, 3)" @=?) =<< [r| v = v1_hs; v[1] <- 7; v |]
    io . assertEqual "" "fromList [1,2,3]" . Prelude.show =<< SVector.unsafeFreeze v1

    let utf8string = "abcd çéõßø"
    io . assertEqual "" utf8string =<< fromSEXP <$> R.cast (sing :: R.SSEXPTYPE 'R.String) <$> [r| utf8string_hs |]

    return ()
