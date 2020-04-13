{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main where

import Control.Lens hiding ((.>), (<|), Unwrapped, Wrapped, para, runFold, (|>))
import qualified Data.Csv as Csv
import qualified Data.Csv.Core as Csv
import Data.Generics.Labels ()
import qualified Data.Text as Text
import NumHask.Prelude hiding ((%), link)
import Options.Generic
import PMLB
import Readme.Lhs
import Test.QuickCheck hiding (output)
import Text.Pretty.Simple (pShowNoColor)

pShow' :: (Show a) => a -> Text
pShow' = toStrict . pShowNoColor

instance ParseRecord (Opts Wrapped)

-- | command line options
data Opts w
  = Opts
      { settype :: w ::: Maybe SetType <?> "classification or regression",
        dataset :: w ::: Maybe Text <?> "dataset to download"
      }
  deriving (Generic)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "pmlb"
  arb :: PMLBConfig <- generate arbitrary
  let ds = fromMaybe (arb ^. #csvConfig . #name) (dataset o)
  let dt = fromMaybe (arb ^. #setType) (settype o)
  let cfg = defaultPMLBConfig & #csvConfig . #name .~ ds & #setType .~ dt
  cacheUrl cfg

  let c = cfg ^. #csvConfig
  putStrLn (ds <> " 👍" :: Text)
  xss <- rights <$> Csv.runE c Csv.doubles
  void
    $ runOutput
      ("other/readme.md_", GitHubMarkdown)
      ("readme.md", GitHubMarkdown)
    $ output "results"
    $ Native
      [ Para
          [Str "dataset: ", link (cfg ^. #csvConfig . #name) (Text.pack $ url cfg)],
        plain "first few lines",
        table "first few lines" [] [] [] (fmap show <$> take 5 xss)
      ]
