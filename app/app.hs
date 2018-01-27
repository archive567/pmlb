{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Lens hiding (Unwrapped, Wrapped, (.>), (<|), (|>), runFold)
import Data.Default
import Data.Generics.Labels ()
import Flow
import Formatting
import Options.Generic
import PMLB
import Protolude hiding ((%))
import Test.QuickCheck
import qualified Data.Text as Text
import qualified Streaming.Prelude as S
import PMLB.Csv
import NumHask.Range
instance ParseRecord (Opts Wrapped)

-- $setup
-- >>> import PMLB
-- >>> import PMLB.Csv

-- | full csv parse of the def config
-- >>> (Right (heads, rs, ())) <- runBS def $ parseCsvFull HasHeader '\t' 500 doubles
-- >>> heads
-- ["X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X21","X22","X23","X24","X25","X26","X27","X28","X29","X30","X31","X32","X33","X34","X35","X36","X37","X38","X39","X40","X41","X42","X43","X44","X45","X46","X47","X48","X49","X50","X51","X52","X53","X54","X55","X56","X57","X58","X59","X60","X61","X62","X63","X64","X65","X66","X67","X68","X69","X70","X71","X72","X73","X74","X75","X76","X77","X78","X79","X80","X81","X82","X83","X84","X85","X86","X87","X88","X89","X90","X91","X92","X93","X94","X95","X96","X97","X98","X99","X100","target"]
--
-- >>> take 2 rs
-- [[39.02,36.49,38.2,38.85,39.38,39.74,37.02,39.53,38.81,38.79,37.65,39.34,38.55,39.03,37.21,36.32,37.81,38.95,36.7,39.72,37.06,37.29,36.43,36.53,36.19,38.17,37.3,36.15,36.68,36.7,36.68,36.99,38.92,37.25,37.47,36.32,35.75,35.68,34.66,34.26,35.62,36.6,34.78,34.67,34.3,33.4,31.4,31.75,31.75,32.84,33.76,35.74,34.01,33.91,36.88,34.41,35.52,36.94,36.95,35.57,38.02,37.32,39.05,37.97,37.01,38.98,38.83,38.87,38.03,38.4,38.25,38.61,36.23,37.81,37.98,38.58,38.96,38.97,39.08,38.79,38.79,36.31,36.59,38.19,37.95,39.63,39.27,37.19,37.13,37.47,37.57,36.62,36.92,38.8,38.52,38.07,36.73,39.46,37.5,39.1,0.0],[1.83,1.71,1.77,1.77,1.68,1.78,1.8,1.7,1.75,1.78,1.86,1.76,1.81,1.86,1.74,1.78,1.81,2.02,2.0,2.01,2.0,2.06,2.0,1.93,1.88,1.85,1.89,1.83,1.76,1.83,1.81,1.81,1.78,1.85,1.86,1.73,1.79,1.81,1.85,1.71,1.71,1.71,1.84,1.76,1.73,1.83,1.68,1.73,1.76,1.77,1.72,1.75,1.66,1.76,1.77,1.78,1.63,1.72,1.66,1.67,1.74,1.65,1.74,1.79,1.69,1.76,1.74,1.82,1.78,1.65,1.65,1.82,1.71,1.83,1.72,1.63,1.77,1.69,1.81,1.74,1.7,1.72,1.74,1.72,1.74,1.71,1.7,1.83,1.79,1.78,1.71,1.8,1.79,1.77,1.74,1.74,1.8,1.78,1.75,1.69,1.0]]
--

-- | no error parse
-- >>> (heads, rows) <- runBS def (parseCsv HasHeader 2 '\t' doubles)
-- >>> take 2 heads
-- ["X1","X2"]
-- 
-- >>> take 2 <$> rows
-- [[39.02,36.49],[1.83,1.71]]
--

-- | column selection example
-- >>> (heads, rs) <- runBS def (parseCols HasHeader 5 '\t' (const double) (101,[2,3,5,100]))
-- >>> heads
-- ["X3","X4","X6","target"]
--
-- >>> rs
-- [[38.2,38.85,39.74,0.0],[1.77,1.77,1.78,1.0],[72981.88,74304.33,69367.34,1.0],[40728.46,38576.36,47034.0,0.0],[5.28,5.38,5.61,0.0]]
--

-- | command line options
data Opts w = Opts
  { datatype :: w ::: Maybe SetType <?> "classification or regression"
  , dataset :: w ::: Maybe Text <?> "dataset to download"
  } deriving (Generic)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "pmlb"
  arb :: Config <- generate arbitrary
  let ds = fromMaybe (arb ^. #dataSet) (dataset o)
  let dt = fromMaybe (arb ^. #dataType) (datatype o)
  let cfg = def |> #dataSet .~ ds |> #dataType .~ dt
  res <- S.take 5 .> S.toList_ |> runLine cfg
  recs <- runBS arb (bsCheck def 1000)
  hs <- runBS arb (parseCsvHeader_ (arb ^. #csep))
  ranges <- runFold arb 1000 doubles rangeFold
  freqs <- discreteFreqCounts arb 5
  putStrLn (ds <> " 👍" :: Text)
  writeFile "other/uptohere.md" <| Text.unlines <|
    [ "random dataset:"
    , sformat
      ("[" %stext % "](" %stext % ")")
      (cfg ^. #dataSet)
      (Text.pack (url cfg))
    , ""
    , "rectangle check (max 1000 lines):"
    , show recs
    , ""
    , "first few lines:"
    ] <> codeWrap res <>
    [ ""
    , "ranges"
    ] <> codeWrap
    [ tabify (["column:"] <> hs)
    , tabify (["min:"] <> (show <$> (\(Range l _) -> l) <$> ranges))
    , tabify (["max:"] <> (show <$> (\(Range _ u) -> u) <$> ranges))
    ] <>
    [ "frequency counts for discrete columns"
    , ""
    ] <> codeWrap (show <$> freqs)

codeWrap :: [Text] -> [Text]
codeWrap ts = ["", "```"] <> ts <> ["```", ""]

tabify :: [Text] -> Text
tabify = Text.intercalate "\t"
