{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PMLB
  ( SetType(..)
  , Config(Config)
  , url
  , file
  , tsep
  , bsep
  , runBS
  , runLine
  , runProducer
  , toLineStream
  , bytes
  , module Data.Default
  ) where

import Control.Lens
       hiding (Unwrapped, Wrapped, (.>), (|>), elements)
import Control.Monad.Managed (Managed, managed, runManaged, with)
import Data.Attoparsec.ByteString.Streaming (Message, parsed)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Streaming.Char8 as B
import qualified Data.ByteString.Streaming.HTTP as HTTP
import Data.Default (Default(..))
import Data.Generics.Labels ()
import Data.String (String)
import qualified Data.Text as Text
import Flow ((.>), (|>))
import Options.Generic (ParseField)
import PMLB.Csv
import PMLB.DataSets
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Protolude
import qualified Streaming.Prelude as S
import Streaming.Zip (gunzip)
import System.Directory (doesFileExist)
import Test.QuickCheck (Arbitrary(..), elements, frequency)

data SetType
  = Classification
  | Regression
  deriving (Show, Generic, Eq, Read, ParseField)

data Config = Config
  { githubUrl :: Text
  , dataType :: SetType
  , dataSet :: Text
  , suffixRemote :: Text
  , suffixLocal :: Text
    -- ^ data is stored uncompressed locally to fit in with Frames methods
    -- since a csv file is needed locally to discover types, remote data is always cached.
  , localCache :: Text
  , csep :: Char
  } deriving (Show, Generic, Eq)

instance Default Config where
  def =
    Config
      "https://github.com/EpistasisLab/penn-ml-benchmarks/raw/master/datasets"
      Classification
      "Hill_Valley_with_noise"
      ".tsv.gz"
      ".csv"
      "./other"
      '\t'

url :: Config -> FilePath
url (Config gh dt ds rsuff _ _ _) =
  gh <> "/" <> Text.toLower (show dt) <> "/" <> ds <> "/" <> ds <> rsuff |>
  Text.unpack

file :: Config -> FilePath
file (Config _ dt ds _ lsuff lc _) =
  lc <> "/" <> Text.toLower (show dt) <> "/" <> ds <> lsuff |> Text.unpack

tsep :: Config -> Text
tsep cfg = csep cfg |> Text.singleton

bsep :: Config -> ByteString
bsep cfg = csep cfg |> C.singleton

-- | resources
withUrlStream :: String -> Managed (B.ByteString IO ())
withUrlStream u =
  managed $ \f -> do
    req <- HTTP.parseRequest u
    man <- HTTP.newManager HTTP.tlsManagerSettings
    HTTP.withHTTP req man $ \resp -> f (HTTP.responseBody resp)

withFileAppend :: FilePath -> Managed Handle
withFileAppend f = managed (withFile f AppendMode)

withFileStream :: MonadIO m => FilePath -> Managed (B.ByteString m ())
withFileStream fp = managed $ \f -> withFile fp ReadMode (f . B.hGetContents)

toCache :: Config -> IO ()
toCache cfg =
  runManaged $ do
    inUrl <- withUrlStream (url cfg)
    outFile <- withFileAppend (file cfg)
    inUrl |> gunzip |> B.hPut outFile |> liftIO

cacheUrl :: Config -> IO ()
cacheUrl cfg = do
  e <- file cfg |> doesFileExist
  when (not e) (toCache cfg)

withStream :: Config -> Managed (B.ByteString IO ())
withStream cfg = do
  liftIO $ cacheUrl cfg
  withFileStream (file cfg)

-- * continuation processing
-- | bytestring stream continuation
-- >>> bytes |> runBS def
-- 839165
--
runBS :: Config -> (B.ByteString IO () -> IO r) -> IO r
runBS cfg = with (withStream cfg)

-- | take a ByteString (A streaming library bytestring) and make a text stream separated by the usual end-of-line conventions.
-- this will break if the dataset includes linebreaks within a quoted text field
toLineStream :: Monad m => Int -> B.ByteString m r -> S.Stream (S.Of Text) m ()
toLineStream n s = s
  |> B.lines
  |> B.denull
  |> S.take n
  |> S.mapped B.toStrict -- the strict wall of pain
  |> S.map decodeUtf8
  |> S.filter (/= "")

-- | text line continuation
-- >>> S.length .> fmap S.fst' |> runLine def
-- 1213
--
runLine :: Config -> (S.Stream (S.Of Text) IO () -> IO r) -> IO r
runLine cfg s = toLineStream 100000 .> s |> runBS cfg

-- | pipes line continuation
-- >>> runProducer def P.length
-- 1213
--
runProducer :: Config -> (P.Producer Text IO () -> IO r) -> IO r
runProducer cfg p = P.unfoldr S.next .> p |> runLine cfg

-- * various computation streams
-- | number of raw bytes
bytes :: B.ByteString IO () -> IO Int
bytes s = s |> B.length |> fmap S.fst'

instance Arbitrary Config where
  arbitrary =
    frequency
      [ ( 1
        , (\x -> def |> #dataType .~ Classification |> #dataSet .~ x) <$>
          elements classificationNames)
      , ( 1
        , (\x -> def |> #dataType .~ Regression |> #dataSet .~ Text.pack x) <$>
          elements regressionNames)
      ]

-- | todo: fixme please
records ::
     (Monad m)
  => Config
  -> B.ByteString m r
  -> m (S.Of [[ByteString]] (Either (Message, B.ByteString m r) r))
records cfg bs = bs |> parsed (record (csep cfg)) |> S.toList
