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

import Control.Lens hiding (Unwrapped, Wrapped, (.>), (<|), (|>))
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

instance ParseRecord (Opts Wrapped)

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
  putStrLn (show res <> " 👍" :: Text)
  writeFile "other/uptohere.md" <| Text.unlines <|
    [ "random dataset:"
    , sformat
      ("[" %stext % "](" %stext % ")")
      (cfg ^. #dataSet)
      (Text.pack (url cfg))
    , "process result:"
    ] <> codeWrap res

codeWrap :: [Text] -> [Text]
codeWrap ts = ["", "```"] <> ts <> ["```", ""]
