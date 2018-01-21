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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Lens hiding (Unwrapped, Wrapped)
import Control.Monad.Managed
import Data.Default
import Data.Generics.Labels ()
import Data.String (String)
import Options.Generic
import Protolude
import Streaming.Zip
import System.Directory
import qualified Data.ByteString.Streaming.Char8 as B
import qualified Data.ByteString.Streaming.HTTP as HTTP
import qualified Data.Text as Text
import qualified Streaming.Prelude as S

data SetType = Classification | Regression deriving (Show, Generic, Eq, Read, ParseField)

show' :: SetType -> Text
show' Classification = "classification"
show' Regression = "regression"

data Caching = UseLocal | UseRemote deriving (Show, Generic, Eq)

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

url :: Config -> FilePath
url (Config gh dt ds s _ _) = Text.unpack $ gh <> "/" <> show' dt <> "/" <> ds <> "/" <> ds <> s

file :: Config -> FilePath
file (Config _ dt ds s _ lc) = Text.unpack $ lc <> "/" <> show' dt <> "/" <> ds <> s

-- | command line options
data Opts w = Opts
  { datatype :: w ::: Maybe SetType <?> "classification or regression"
  , dataset :: w ::: Maybe Text <?> "dataset to download" 
  } deriving (Generic)


-- | resources
withUrlStream :: String -> Managed (B.ByteString IO ())
withUrlStream u = managed $ \f -> do
    req <- HTTP.parseRequest u
    man <- HTTP.newManager HTTP.tlsManagerSettings
    HTTP.withHTTP req man $ \resp -> f (HTTP.responseBody resp)

withFileAppend :: FilePath -> Managed Handle
withFileAppend f = managed (withFile f AppendMode)

withFileStream :: MonadIO m => FilePath -> Managed (B.ByteString m ())
withFileStream fp = managed $ \f ->
  withFile fp ReadMode (f . B.hGetContents)

toCache :: Config -> IO ()
toCache cfg = runManaged $ do
    inUrl <- withUrlStream (url cfg)
    outFile <- withFileAppend (file cfg)
    liftIO $ inUrl & B.hPut outFile

cacheUrl :: Config -> IO ()
cacheUrl cfg = case useCache cfg of
  UseLocal -> do
      e <- doesFileExist $ file cfg
      when (not e) (toCache cfg)
  UseRemote -> pure ()

withStream :: Config -> Managed (B.ByteString IO ())
withStream cfg = 
    case useCache cfg of
      UseRemote -> withUrlStream (url cfg)
      UseLocal -> do
          liftIO $ cacheUrl cfg
          withFileStream (file cfg)


-- | main process
run :: Config -> (B.ByteString IO () -> IO r) -> IO r
run cfg s = with (withStream cfg) $ s . gunzip

-- * different processings

bytes :: B.ByteString IO () -> IO Text
bytes s = do
    (l S.:> _) <- s & B.length
    pure $ "byte length " <> show l

lineCount :: B.ByteString IO () -> IO Int
lineCount s = do
    (c S.:> _) <- s & lineStream 100000 & S.length
    pure c

-- * different streams

-- | take a ByteString (A streaming library bytestring) and make a text stream
lineStream :: Monad m => Int -> B.ByteString m r -> S.Stream (S.Of Text) m ()
lineStream n s =
    s &
    B.lines &
    B.denull &
    S.take n &
    S.mapped B.toStrict & -- the strict wall of pain
    S.map decodeUtf8 &
    -- S.concat &
    S.filter (/="")

lines :: Int -> B.ByteString IO () -> IO [Text]
lines n s =
    s & lineStream n & S.toList_

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "pmlb"
  let ds = fromMaybe (dataSet def) (dataset o)
  let dt = fromMaybe (dataType def) (datatype o)
  let cfg = #dataSet .~ ds $ #dataType .~ dt $ def
  let process = lines 2
  res <- run cfg process
  putStrLn (show res <> " 👍" :: Text)
  writeFile
    "other/uptohere.md"
    ( "last dataset used:\n" <>
      "[" <> (cfg ^. #dataSet) <> "]" <> "(" <> Text.pack (url cfg) <> ")\n" <>
      "\n" <> "process result:" <>
      codeWrap res)

codeWrap :: [Text] -> Text
codeWrap ts = "\n```\n" <> Text.intercalate "\n" ts <> "\n```\n"

-- | doctests
-- >>> run def bytes
-- "byte length 839165"
--
-- >>> run def lineCount
-- 1213
--
