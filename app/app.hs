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
import Frames.CSV
-- import Data.Vinyl
import Frames.ColumnTypeable
import qualified Pipes.Prelude.Text as PT
import Frames.ColumnUniverse

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

instance ParseRecord (Opts Wrapped)

url :: Config -> FilePath
url (Config gh dt ds rsuff _  _ _) = Text.unpack $ gh <> "/" <> show' dt <> "/" <> ds <> "/" <> ds <> rsuff

file :: Config -> FilePath
file (Config _ dt ds _ lsuff lc _) = Text.unpack $ lc <> "/" <> show' dt <> "/" <> ds <> lsuff

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

withProducer :: Config -> (P.Producer Text IO () -> IO r) -> IO r
withProducer cfg p = withLineStream cfg $ p . P.unfoldr S.next 

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
  let ds = fromMaybe (dataSet def) (dataset o)
  let dt = fromMaybe (dataType def) (datatype o)
  let cfg = #dataSet .~ ds $ #dataType .~ dt $ def
  let process = lines 2
  res <- withBS cfg process
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
-- >>> withBS def bytes
-- "byte length 839165"
--
-- >>> withLineStream def (fmap S.fst' . S.length)
-- 1213
--
-- >>> rs <- withProducer def (readRows def)
-- >>> :t rs
-- rs :: [Row]
--
-- >>> length rs
-- 1212
--
