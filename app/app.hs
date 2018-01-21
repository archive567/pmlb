{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Lens hiding (Unwrapped, Wrapped)
import Data.Default
import Data.Generics.Labels ()
import Options.Generic
import Protolude
import qualified Data.ByteString.Streaming.Char8 as B
import qualified Data.Text as Text
import qualified Streaming.Prelude as S
import qualified Pipes.Prelude as P
import qualified Pipes as P
import Frames.CSV
-- import Data.Vinyl
import Frames.ColumnTypeable
import qualified Pipes.Prelude.Text as PT
import Frames.ColumnUniverse
import PMLB.Config
import Test.QuickCheck

instance ParseRecord (Opts Wrapped)

-- | command line options
data Opts w = Opts
  { datatype :: w ::: Maybe SetType <?> "classification or regression"
  , dataset :: w ::: Maybe Text <?> "dataset to download" 
  } deriving (Generic)

-- * stream experiments
bytes :: B.ByteString IO () -> IO Text
bytes s = do
    (l S.:> _) <- s & B.length
    pure $ "byte length " <> show l

lineCount :: B.ByteString IO () -> IO Int
lineCount s = do
    (c S.:> _) <- s & lineStream 100000 & S.length
    pure c

lines :: Int -> B.ByteString IO () -> IO [Text]
lines n s =
    s & lineStream n & S.toList_

-- * csv discovery
cfgRowGen :: Config -> RowGen a
cfgRowGen cfg = RowGen [] "" (sep cfg) "Row" Proxy (PT.readFileLn (file cfg))

tableType' (RowGen [] "" "\t" "Row" Proxy (PT.readFileLn "other/classification/Hill_Valley_with_noise.csv") :: RowGen Columns)

readRows :: (Monad m) => Config -> P.Producer Text m () -> m [Row]
readRows cfg p = P.toListM $ p P.>-> pipeTableOpt (ParserOptions Nothing (sep cfg) NoQuoting)

readCols :: (Monad m, Monoid a, ColumnTypeable a) => P.Producer Text m () -> m [(Text, a)]
readCols p = p & readColHeaders (ParserOptions Nothing "\t" NoQuoting)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "pmlb"
  arb <- generate arbitrary
  let ds = fromMaybe (dataSet arb) (dataset o)
  let dt = fromMaybe (dataType arb) (datatype o)
  let cfg = #dataSet .~ ds $ #dataType .~ dt $ def
  let process = lines 2
  res <- withBS cfg process
  putStrLn (show res <> " 👍" :: Text)
  writeFile
    "other/uptohere.md"
    ( "random dataset:\n" <>
      "[" <> (cfg ^. #dataSet) <> "]" <> "(" <> Text.pack (url cfg) <> ")\n" <>
      "\n" <> "process result:" <>
      codeWrap res)

codeWrap :: [Text] -> Text
codeWrap ts = "\n```\n" <> Text.intercalate "\n" ts <> "\n```\n"

-- | doctests
-- >>> withBS def bytes
-- "byte length 839165"
--
-- >>> withLineStream def (fmap S.fst' . S.length)
-- 1213
--
-- > rs <- withProducer def (readRows def)
--
-- > :t rs
-- rs :: [Row]
--
-- > length rs
-- 1212
--
