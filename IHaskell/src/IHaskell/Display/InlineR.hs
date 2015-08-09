-- |
-- Copyright: 2015 (C) Tweag I/O Limited

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module IHaskell.Display.InlineR
  ( initializeEmbeddedR
  , Config(..)
  , defaultConfig
  , r
  , rprint
  , rgraph
  , Language.R.Instance.runRegion
  ) where

import           Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char
import           Data.Monoid
import           H.Prelude.Interactive as H -- we use provide instances to IO Monad
import           IHaskell.Display
import           IHaskell.Display.Blaze () -- to confirm it's installed
import           Language.Haskell.TH.Quote
import           Language.R.Instance
import           Language.R.QQ
import           System.Directory
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BH

rprint :: QuasiQuoter
rprint = QuasiQuoter { quoteExp = \s -> [| do H.p $(quoteExp r s) |] }

rgraph :: QuasiQuoter
rgraph = QuasiQuoter { quoteExp = \s ->
    [| do idx <- findMaxIndex 0
          let fname = mkName idx
          _ <- [r| png(filename=fname_hs, width=480, height=480, bg="white"); |]
          H.p $(quoteExp r s)
          _ <- [r| dev.off() |]
          encoded <- Base64.encode <$> B.readFile fname
          display $ BH.img BH.! BH.src
                                 (BH.unsafeByteStringValue
                                    (Char.pack "data:image/png;base64," <> encoded))
     |] }

mkName :: Int -> FilePath
mkName i = "Rplots/auto" <> Prelude.show i <> ".png"

findMaxIndex :: Int -> IO Int
findMaxIndex i = do
  b <- doesFileExist $ mkName i
  if b then findMaxIndex (i+1)
       else return i

-- | Initialize Embedded R process and configure R
initializeEmbeddedR :: Config -> IO ()
initializeEmbeddedR config = do
  H.initialize config
  -- Configure plots output
  createDirectoryIfMissing False "Rplots"
