{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PMLB
  ( SetType (..),
    CsvConfig (..),
    defaultCsvConfig,
    PMLBConfig (..),
    defaultPMLBConfig,
    url,
    file,
    tsep,
    bsep,
    cacheUrl,
    runBS,
    runLine,
    runProducer,
    runRows,
    toLineStream,
    bytes,
    bsCheck,
    rangeFold,
    runFold,
    discreteFreqCounts,
  )
where

import Control.Category (id, (>>>))
import qualified Control.Foldl as L
import Control.Lens hiding (Unwrapped, Wrapped, elements, runFold, (|>))
import Control.Monad.Managed (Managed, managed, runManaged, with)
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Streaming as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Streaming.Char8 as B
import qualified Data.ByteString.Streaming.HTTP as HTTP
import Data.Generics.Labels ()
import qualified Data.Map as Map
import Data.Scientific
import Data.String (String)
import qualified Data.Text as Text
import NumHask.Space
import Options.Generic (ParseField)
import PMLB.Csv as C
import PMLB.DataSets
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Protolude
import qualified Streaming.Prelude as S
import Streaming.Zip (gunzip)
import System.Directory (doesFileExist)
import Test.QuickCheck (Arbitrary (..), elements, frequency)

data SetType
  = Classification
  | Regression
  deriving (Show, Generic, Eq, Read, ParseField)

data CsvConfig
  = CsvConfig
      { dataSet :: Text,
        suffix :: Text,
        localCache :: Text,
        csep :: Char,
        -- | length of the continuating bytestream to report on an error
        errStream :: Int,
        -- | maximum number of lines when splitting the stream
        maxLines :: Int,
        -- | number of rows to get initial statistical calcs with
        firstRows :: Int
      }
  deriving (Show, Generic, Eq)

defaultCsvConfig :: CsvConfig
defaultCsvConfig =
  CsvConfig
    "Hill_Valley_with_noise"
    ".csv"
    "./other"
    '\t'
    500
    100000
    100

data PMLBConfig =
  PMLBConfig
  { setType :: SetType
  , urlRoot :: Text
  , suffixRemote :: Text
  , csvConfig :: CsvConfig
  } deriving (Eq, Show, Generic)

defaultPMLBConfig :: PMLBConfig
defaultPMLBConfig =
  PMLBConfig
  Classification
  "https://github.com/EpistasisLab/penn-ml-benchmarks/raw/master/datasets"
  ".tsv.gz"
  (CsvConfig
    "Hill_Valley_with_noise"
    ".csv"
    "./other"
    '\t'
    500
    100000
    100)


url :: PMLBConfig -> FilePath
url cfg =
  cfg ^. #urlRoot <> "/"
    <> Text.toLower (show (cfg ^. #setType)) <> "/"
    <> cfg ^. #csvConfig . #dataSet
    <> "/"
    <> cfg ^. #csvConfig . #dataSet
    <> cfg ^. #suffixRemote
    & Text.unpack

file :: CsvConfig -> FilePath
file cfg =
  cfg ^. #localCache
    <> "/"
    <> cfg ^. #dataSet
    <> cfg ^. #suffix
      & Text.unpack

tsep :: CsvConfig -> Text
tsep cfg = cfg ^. #csep & Text.singleton

bsep :: CsvConfig -> ByteString
bsep cfg = cfg ^. #csep & C.singleton

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

toCache :: PMLBConfig -> IO ()
toCache cfg =
  runManaged $ do
    inUrl <- withUrlStream (url cfg)
    outFile <- withFileAppend (file (cfg ^. #csvConfig))
    inUrl & gunzip & B.hPut outFile & liftIO

cacheUrl :: PMLBConfig -> IO ()
cacheUrl cfg = do
  e <- file (cfg ^. #csvConfig) & doesFileExist
  unless e (toCache cfg)

withStreamPMLB :: PMLBConfig -> Managed (B.ByteString IO ())
withStreamPMLB cfg = do
  liftIO $ cacheUrl cfg
  withFileStream (file (cfg ^. #csvConfig))

-- * continuation processing

-- | bytestring stream continuation
--
-- >>> bytes & runBS defaultCsvConfig
-- 839165
runBS :: CsvConfig -> (B.ByteString IO () -> IO r) -> IO r
runBS cfg = with (withFileStream (file cfg))

-- | bytestring stream continuation
--
-- >>> bytes & runBSPMLB defaultPMLBConfig
-- 839165
runBSPMLB :: PMLBConfig -> (B.ByteString IO () -> IO r) -> IO r
runBSPMLB cfg = with (withStreamPMLB cfg)

-- | take a ByteString (A streaming library bytestring) and make a text stream separated by the usual end-of-line conventions.
-- this will break if the dataset includes linebreaks within a quoted text field
toLineStream :: Monad m => Int -> B.ByteString m r -> S.Stream (S.Of Text) m ()
toLineStream n s =
  s
    & B.lines
    & B.denull
    & S.take n
    & S.mapped B.toStrict -- the strict wall of pain
    & S.map decodeUtf8
    & S.filter (/= "")

-- | text line continuation
--
-- >>> (S.length >>> fmap S.fst') & runLine defaultCsvConfig
-- 1213
runLine :: CsvConfig -> (S.Stream (S.Of Text) IO () -> IO r) -> IO r
runLine cfg s = (toLineStream (cfg ^. #maxLines) >>> s) & runBS cfg

-- | pipes line continuation
--
-- >>> runProducer defaultCsvConfig P.length
-- 1213
runProducer :: CsvConfig -> (P.Producer Text IO () -> IO r) -> IO r
runProducer cfg p = (P.unfoldr S.next >>> p) & runLine cfg

-- | taking a parser, run a stream computation over n rows
--
-- >>> runRows defaultCsvConfig 2 doubles S.toList_ & fmap (fmap (take 4))
-- [[39.02,36.49,38.2,38.85],[1.83,1.71,1.77,1.77]]
runRows :: CsvConfig -> Int -> (Char -> AC.Parser a) -> (S.Stream (S.Of a) IO () -> IO r) -> IO r
runRows cfg n p s = runBS cfg (streamCsv_ HasHeader n (cfg ^. #csep) p s)

-- | taking a parser, run a foldl over n rows
--
-- >>> runFold defaultCsvConfig 1000 doubles rangeFold & fmap (drop 95)
-- [Range 0.89 112037.22,Range 0.89 115110.42,Range 0.86 116431.96,Range 0.91 113291.96,Range 0.89 114533.76,Range 0.0 1.0]
runFold :: CsvConfig -> Int -> (Char -> AC.Parser a) -> L.Fold a b -> IO b
runFold cfg n p f = runBS cfg (streamCsv_ HasHeader n (cfg ^. #csep) p (L.purely S.fold f >>> fmap S.fst'))

-- * various computation streams

-- | number of raw bytes
bytes :: B.ByteString IO () -> IO Int
bytes s = s & B.length & fmap S.fst'

instance Arbitrary PMLBConfig where
  arbitrary =
    frequency
      [ ( 1,
          (\x -> defaultPMLBConfig & #setType .~ Classification & #csvConfig . #dataSet .~ x)
            <$> elements classificationNames
        ),
        ( 1,
          (\x -> defaultPMLBConfig & #setType .~ Regression & #csvConfig . #dataSet .~ x)
            <$> elements regressionNames
        )
      ]

-- | records of a csv as raw bytestrings
--
-- >>> (Right recs) <- runBS defaultCsvConfig (bsAll defaultCsvConfig)
-- >>> recs & all (length >>> (== 101))
-- True
--
-- >>> recs & length
-- 1213
bsAll ::
  Monad m =>
  CsvConfig ->
  B.ByteString m () ->
  m (Either (Text, ByteString) [[ByteString]])
bsAll cfg bs = do
  res <- parseCsvBody (csep cfg) (cfg ^. #errStream) C.record bs
  case res of
    Left err -> pure $ Left err
    Right (bss, _) -> pure $ Right bss

-- check bytestring for rectangularity
--
-- >>> bsCheck defaultCsvConfig 100 (B.fromLazy "a\tb\tc\nd\te\tf\n")
-- Right (2,3)
--
-- >>>  bsCheck defaultCsvConfig 100 (B.fromLazy "a\tb\tc\nd\te\n")
-- Left "non-equal record length"
--
-- >>> bsCheck defaultCsvConfig 100 (B.fromLazy "")
-- Left "empty: "
--
-- >>> bsCheck defaultCsvConfig 100 (B.fromLazy "\n")
-- Right (1,1)
--
bsCheck :: (Monad m) => CsvConfig -> Int -> B.ByteString m r -> m (Either Text (Int, Int))
bsCheck cfg n bs = do
  res <- parseCsv_ NoHeader n (csep cfg) C.record bs
  case res of
    [] -> "empty: prolly a parser issue" & Left & pure
    (x : xs) ->
      if all (\y -> length y == length x) xs
        then (length xs + 1, length x) & Right & pure
        else "non-equal record lengths" & Left & pure

-- | an issue with the PMLB datasets is that it is not immediately obvious which columns are:
--
-- - Int: Discrete but an Ord instance, and/or where magnitude is meaningful
-- - Categorical: not an Ord and magnitude of the Int isn't meaningful
-- - Binary: No way of telling if 0,1 values are boolean until you have all the rows
data V = Continuous Float | Discrete Int | Binary Bool deriving (Show)

-- | convert a Scientific value to a V
toV :: Scientific -> V
toV s = case floatingOrInteger s of
  Left r -> Continuous r
  Right i -> Discrete i

isDiscrete :: V -> Bool
isDiscrete (Discrete _) = True
isDiscrete (Continuous _) = False
isDiscrete (Binary _) = False

rangeFold :: (Ord a) => L.Fold [a] [Range a]
rangeFold = L.Fold step [] id
  where
    step [] as = (\a -> Range a a) <$> as
    step rs as = zipWith (<>) rs $ (\a -> Range a a) <$> as

discreteCols :: CsvConfig -> Int -> IO [Int]
discreteCols cfg n = do
  rs <- runBS cfg (parseCsv_ HasHeader n (cfg ^. #csep) scis)
  rs & transpose & fmap (all (isDiscrete . toV))
    & zip [0 ..]
    & filter snd
    & fmap fst
    & pure

countFold :: (Ord a) => L.Fold a (Map.Map a Int)
countFold = L.Fold step Map.empty identity
  where
    step x a = Map.insertWith (+) a 1 x

countsFold :: (Ord a) => Int -> L.Fold [a] [Map.Map a Int]
countsFold n = L.Fold step (replicate n Map.empty) identity
  where
    step = zipWith (\x a -> Map.insertWith (+) a 1 x)

nrows :: CsvConfig -> IO Int
nrows cfg =
  runBS cfg (parseCsvHeader_ (cfg ^. #csep)) & fmap length

discreteFreqCounts :: CsvConfig -> Int -> IO [(Text, [(Scientific, Int)])]
discreteFreqCounts cfg topn = do
  nr <- nrows cfg
  dc <- discreteCols cfg (cfg ^. #firstRows)
  freqs <- runFold cfg (cfg ^. #maxLines) (cols (const AC.scientific) nr dc) (countsFold nr)
  (Left hs, _) <- runBS cfg (S.parse (cols field nr dc (cfg ^. #csep)))
  pure $ zip (decodeUtf8 <$> hs) $ take topn . sortOn (Down . snd) . Map.toList <$> freqs
