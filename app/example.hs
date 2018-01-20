{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall #-}

import Options.Generic
import Protolude
import Data.Default
import Control.Lens hiding (Unwrapped, Wrapped)
import Data.Generics.Labels ()

data SetType = Classification | Regression deriving (Show, Generic, Eq, Read, ParseField)

data Caching = UseLocal | UseRemote deriving (Show, Generic, Eq)

data Opts w = Opts
  { datatype :: w ::: Maybe SetType <?> "classification or regression"
  , dataset :: w ::: Maybe Text <?> "dataset to download" 
  } deriving (Generic)

data Config = Config
  { githubUrl :: Text
  , dataType :: SetType
  , dataSet :: Text
  , suffix :: Text
  , useCache :: Caching
  , localCache :: Text
  } deriving (Show, Generic, Eq)

instance Default Config where
  def =
    Config
    "https://github.com/EpistasisLab/penn-ml-benchmarks/raw/master/datasets"
    Classification
    "Hill_Valley_with_noise"
    ".tsv.gz"
    UseLocal
    "./other"

instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "pmlb"
  let ds = fromMaybe (dataSet def) (dataset o)
  let dt = fromMaybe (dataType def) (datatype o)
  let cfg = (def :: Config) -- & #dataSet .~ ds -- & #dataType .~ dt
  putStrLn (show " 👍" :: Text)
  writeFile
    "other/fulldataset.md"
    ("dataset being downloaded is " <> show cfg)

-- | doctests
-- > coming soon
--
