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


module PMLB.Config where

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
import qualified Pipes.Prelude as P
import qualified Pipes as P
import PMLB.DataSets
import Test.QuickCheck

data SetType = Classification | Regression deriving (Show, Generic, Eq, Read, ParseField)

show' :: SetType -> Text
show' Classification = "classification"
show' Regression = "regression"

data Config = Config
  { githubUrl :: Text
  , dataType :: SetType
  , dataSet :: Text
  , suffixRemote :: Text
  , suffixLocal :: Text
    -- ^ data is stored uncompressed locally to fit in with Frames methods
    -- since a csv file is needed locally to discover types, remote data is always cached.
  , localCache :: Text
  , sep :: Text
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
    "\t"

url :: Config -> FilePath
url (Config gh dt ds rsuff _  _ _) = Text.unpack $ gh <> "/" <> show' dt <> "/" <> ds <> "/" <> ds <> rsuff

file :: Config -> FilePath
file (Config _ dt ds _ lsuff lc _) = Text.unpack $ lc <> "/" <> show' dt <> "/" <> ds <> lsuff

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
    liftIO $ inUrl & gunzip & B.hPut outFile

cacheUrl :: Config -> IO ()
cacheUrl cfg = do
      e <- doesFileExist $ file cfg
      when (not e) (toCache cfg)

withStream :: Config -> Managed (B.ByteString IO ())
withStream cfg = do
    liftIO $ cacheUrl cfg
    withFileStream (file cfg)

-- | main processing
withBS :: Config -> (B.ByteString IO () -> IO r) -> IO r
withBS cfg = with (withStream cfg)

withLineStream :: Config -> (S.Stream (S.Of Text) IO () -> IO r) -> IO r
withLineStream cfg s = withBS cfg $ s . lineStream 100000

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

withProducer :: Config -> (P.Producer Text IO () -> IO r) -> IO r
withProducer cfg p = withLineStream cfg $ p . P.unfoldr S.next 


instance Arbitrary Config where
    arbitrary =
        frequency
            [ (1, (\x ->   #dataType .~ Classification
                        $ #dataSet .~ x
                        $ def) <$>
                Test.QuickCheck.elements classificationNames)
            , (1, (\x ->   #dataType .~ Regression
                        $ #dataSet .~ Text.pack x
                        $ def) <$>
                Test.QuickCheck.elements regressionNames)
            ]
