{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module PMLB.Csv
  ( lineEnd,
    sep,
    sep',
    field,
    skipField,
    oneRecord,
    record,
    scis,
    AC.scientific,
    doubles,
    double,
    Header (..),
    parseCsvFull,
    parseCsvHeader,
    parseCsvHeader_,
    parseCsvBody,
    parseCsvBody_,
    parseCsv,
    parseCsv_,
    skipHead,
    Skippy (..),
    fromSkips,
    toSkips,
    cols,
    parseCols,
    streamCsv_,
  )
where

import qualified Control.Foldl as L
import Data.Attoparsec.ByteString.Char8 as AC
import Data.Attoparsec.ByteString.Streaming as S
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Streaming.Char8 as B
import Data.Scientific
import Protolude
import qualified Streaming.Prelude as S

-- * low-level generic csv parser helpers

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoImplicitPrelude

-- >>> AC.parse lineEnd "\r\nok"
-- Done "ok" ()
--
-- >>> let (Partial x) = AC.parse lineEnd "\r"
-- >>> x "\nok"
-- Done "ok" ()
--
lineEnd :: AC.Parser ()
lineEnd =
  void (AC.char '\n') <|> void (string "\r\n")
    <|> void (AC.char '\r') <?> "end of line"

-- | Most parsing and building routines implicity assume comma separated, and newlines separating rows.
--
-- >>> AC.parse (sep '\t') "\tok"
-- Done "ok" ()
sep :: Char -> AC.Parser ()
sep c = void (AC.char c)

-- | for when you need to treat eol as a sep
--
-- >>> AC.parse (sep' '\t') "\nok"
-- Done "ok" ()
sep' :: Char -> AC.Parser ()
sep' c = sep c <|> lineEnd

-- | an unquoted field
-- Does not consume the separator token
-- >>> AC.parse (field ',') "field,ok"
-- Done ",ok" "field"
field :: Char -> AC.Parser ByteString
field c = AC.takeWhile (`C.notElem` (C.singleton c <> "\n\r")) <?> "unquoted field"

-- | skipping a field
-- >>> AC.parse (skipField ',') "field,ok"
-- Done ",ok" ()
skipField :: Char -> AC.Parser ()
skipField c = AC.skipWhile (`C.notElem` (C.singleton c <> "\n\r")) <?> "unquoted field"

-- | There are two slightly different situations when consuming a line of csv:
--
-- - `lines` has been applied, and `lineEnds` are absent from the stream
--
-- - lines has not been applied, and finding a lineEnd token is what separates csv lines.
--
-- oneRecord doesn't look for an endLine token
-- >>> AC.parse (oneRecord ',') "field1,field2\nok"
-- Done "\nok" ["field1","field2"]
oneRecord :: Char -> AC.Parser [ByteString]
oneRecord c = field c `sepBy1` sep c

-- | looks for and consumes an endLine token
-- >>> AC.parse (record ',') "field1,field2\nok"
-- Done "ok" ["field1","field2"]
record :: Char -> AC.Parser [ByteString]
record c = field c `sepBy1` sep c <* lineEnd

-- | record parser for a csv row of all scientifics
-- >>> AC.parse (scis ',') "1,2.2,3.3\nok"
-- Done "ok" [1.0,2.2,3.3]
scis :: Char -> AC.Parser [Scientific]
scis c = AC.scientific `sepBy1` sep c <* lineEnd

-- | record parser for a csv row of all scientifics
-- >>> AC.parse (doubles ',') "1,2,3\nok"
-- Done "ok" [1.0,2.0,3.0]
doubles :: Char -> AC.Parser [Double]
doubles c = AC.double `sepBy1` sep c <* lineEnd

-- | does the csv have a header row?
data Header = HasHeader | NoHeader deriving (Show, Eq)

-- | parse an entire csv file, retaining all messages.
-- necessary as a separate function as take n in a stream tends to splat messages etc
-- >>> parseCsvFull HasHeader ',' 10 doubles (B.fromStrict "h1,h2,\n0,1\n")
-- Right (["h1","h2",""],[[0.0,1.0]],())
parseCsvFull ::
  Monad m =>
  Header ->
  Char ->
  Int ->
  (Char -> Parser a) ->
  B.ByteString m r ->
  -- | return either an error message, together with a sample of the next stream portion, or the headers (if HasHeaders), a list of parsed rows, and the return value of the stream, phew
  m (Either (Text, ByteString) ([Text], [a], r))
parseCsvFull h c n p bs =
  case h of
    HasHeader -> do
      eh <- parseCsvHeader c n bs
      case eh of
        Left err -> pure $ Left err
        Right (heads, bs') -> do
          r <- parseCsvBody c n p bs'
          case r of
            Left err -> pure $ Left err
            Right (rs, r') -> pure $ Right (heads, rs, r')
    NoHeader -> do
      r <- parseCsvBody c n p bs
      case r of
        Left err -> pure $ Left err
        Right (rs, r') -> pure $ Right ([], rs, r')

grab :: (Monad m, Integral a) =>
              B.ByteString m r -> a -> m ByteString
grab b n = b & B.take (fromIntegral n) & B.toStrict & fmap S.fst'

-- | parse the body of a csv
-- >>>  parseCsvBody ',' 10 doubles (B.fromStrict "0,1\n2,3\n")
-- Right ([[0.0,1.0],[2.0,3.0]],())
parseCsvBody ::
  Monad m =>
  Char ->
  Int ->
  (Char -> Parser a) ->
  B.ByteString m r ->
  m (Either (Text, ByteString) ([a], r))
parseCsvBody c n p bs = do
  (rs S.:> overs) <- bs & parsed (p c) & S.toList
  case overs of
    Left (msg, bs') -> do
      bs'' <- grab bs' n
      ("parsed error: " <> show msg, bs'') & Left & pure
    Right r -> (rs, r) & Right & pure

-- | parse a header row
-- >>> (Right (heads,_)) <- parseCsvHeader ',' 10 (B.fromStrict "h1,h2\n0,1\n")
-- >>> heads
-- ["h1","h2"]
parseCsvHeader ::
  Monad m =>
  Char ->
  Int ->
  B.ByteString m r ->
  m (Either (Text, ByteString) ([Text], B.ByteString m r))
parseCsvHeader c n bs = do
  (eHeaders, bs') <- bs & S.parse (record c)
  case eHeaders of
    Right err -> do
      bs'' <- grab bs' n
      ("header error: " <> show err, bs'') & Left & pure
    Left heads -> (decodeUtf8 <$> heads, bs') & Right & pure

-- | parse a header row, with no continuation or error checking
-- >>> parseCsvHeader_ ',' (B.fromStrict "h1,h2\n0,1\n")
-- ["h1","h2"]
parseCsvHeader_ ::
  Monad m =>
  Char ->
  B.ByteString m r ->
  m [Text]
parseCsvHeader_ c bs = do
  (eHeaders, _) <- bs & S.parse (record c)
  case eHeaders of
    Right _ -> pure []
    Left heads -> decodeUtf8 <$> heads & pure

-- | parse n rows of the body of a csv
-- >>> parseCsvBody_ 2 ',' doubles (B.fromStrict "0,1\n2,3\n4,5\n")
-- [[0.0,1.0],[2.0,3.0]]
parseCsvBody_ ::
  Monad m =>
  Int ->
  Char ->
  (Char -> Parser a) ->
  B.ByteString m r ->
  m [a]
parseCsvBody_ n c p bs = bs & parsed (p c) & S.take n & S.toList_

-- | run a stream computation over n rows of the body of a csv
-- >>> streamCsvBody_ 2 ',' doubles S.toList_ (B.fromStrict "0,1\n2,3\n4,5\n")
-- [[0.0,1.0],[2.0,3.0]]
streamCsvBody_ ::
  Monad m =>
  Int ->
  Char ->
  (Char -> Parser a) ->
  (S.Stream (S.Of a) m () -> b) ->
  B.ByteString m r ->
  b
streamCsvBody_ n c p s bs = bs & parsed (p c) & S.take n & s

-- | skip over a header row
skipHead :: Monad m => Char -> B.ByteString m r -> m (B.ByteString m r)
skipHead c bs = bs & S.parse (record c) & fmap snd

-- | parse n rows of a csv, including a header row, bypassing all errors
-- >>> parseCsv HasHeader 2 ',' doubles (B.fromStrict "h1,h2\n0,1\n2,3\n4,5\n")
-- (["h1","h2"],[[0.0,1.0],[2.0,3.0]])
parseCsv ::
  Monad m =>
  Header ->
  Int ->
  Char ->
  (Char -> Parser a) ->
  B.ByteString m r ->
  m ([Text], [a])
parseCsv h n c p bs =
  case h of
    HasHeader -> do
      hs <- parseCsvHeader c 0 bs
      case hs of
        Left _ -> ([], []) & pure
        Right (heads, bs') -> do
          rs <- parseCsvBody_ n c p bs'
          (heads, rs) & pure
    NoHeader -> do
      rs <- parseCsvBody_ n c p bs
      ([], rs) & pure

-- | parse n rows of a csv, skipping a header row, if one exists, bypassing all errors
-- >>>  parseCsv_ HasHeader 2 ',' doubles (B.fromStrict "h1,h2\n0,1\n2,3\n4,5\n")
-- [[0.0,1.0],[2.0,3.0]]
parseCsv_ ::
  Monad m =>
  Header ->
  Int ->
  Char ->
  (Char -> Parser a) ->
  B.ByteString m r ->
  m [a]
parseCsv_ h n c p bs =
  case h of
    HasHeader -> do
      bs' <- skipHead c bs
      parseCsvBody_ n c p bs'
    NoHeader -> parseCsvBody_ n c p bs

-- | run a stream computation over n rows of a csv, skipping a header row, if one exists, bypassing all errors
-- >>>  streamCsv_ HasHeader 2 ',' doubles S.toList_ (B.fromStrict "h1,h2\n0,1\n2,3\n4,5\n")
-- [[0.0,1.0],[2.0,3.0]]
streamCsv_ ::
  Monad m =>
  Header ->
  Int ->
  Char ->
  (Char -> Parser a) ->
  (S.Stream (S.Of a) m () -> m b) ->
  B.ByteString m r ->
  m b
streamCsv_ h n c p s bs =
  case h of
    HasHeader -> do
      bs' <- skipHead c bs
      streamCsvBody_ n c p s bs'
    NoHeader -> streamCsvBody_ n c p s bs

-- | compression helper for a list of indexes
data Skippy = Skip Int | Retain Int deriving (Show)

-- | convert a list of column indexes to a Skippy
-- >>> toSkips (20, [3,4,12])
-- [Skip 3,Retain 2,Skip 7,Retain 1,Skip 7]
toSkips :: (Int, [Int]) -> [Skippy]
toSkips (n, is) = L.fold (L.Fold step ([], (0, 0)) done) is
  where
    step (f, (r, x)) a
      | a == x = (f, (r + 1, a + 1))
      | r == 0 = (Skip (a - x) : f, (1, a + 1))
      | otherwise = (Skip (a - x) : Retain r : f, (1, a + 1))
    done (f, (r, x))
      | x > n = reverse f
      | x == n && r == 0 = reverse f
      | r == 0 = reverse $ Skip (n - x) : f
      | n == x = reverse $ Retain r : f
      | otherwise = reverse $ Skip (n - x) : Retain r : f

-- | convert a list of Skippys to list of column indexes
-- >>> fromSkips [Skip 3,Retain 2,Skip 7,Retain 1,Skip 7]
-- (20,[3,4,12])
fromSkips :: [Skippy] -> (Int, [Int])
fromSkips = L.fold (L.Fold step (0, []) identity)
  where
    step (x, is) (Skip n) = (x + n, is)
    step (x, is) (Retain n) = (x + n, is <> ((x +) <$> Protolude.take n [0 ..]))

-- | parse specific columns
-- >>> AC.parse (cols (const double) 5 [0,3,4] ',') "0,1,2,3,4,5\n"
-- Done "5\n" [0.0,3.0,4.0]
cols :: (Char -> AC.Parser a) -> Int -> [Int] -> (Char -> AC.Parser [a])
cols p n is = L.fold (L.Fold step (const $ pure []) identity) (toSkips (n, is))
  where
    step x (Retain a) = \c -> (<>) <$> x c <*> mp a c
    step x (Skip a) = \c -> x c <* skipFields a c
    mp m c = replicateM m (p c <* sep' c)
    skipFields m c = replicateM_ m (skipField c <* sep' c)

-- | parse n rows of a csv, filtered to a list of columns
-- >>> parseCols HasHeader 2 ',' (const double) (5, [1,2]) (B.fromStrict "h0,h1,h2,h3,h4\n0,1,2,3,4\n5,6,7,8,9\n10,11,12,13,14")
-- (["h1","h2"],[[1.0,2.0],[6.0,7.0]])
parseCols ::
  Monad m =>
  Header ->
  Int ->
  Char ->
  (Char -> Parser a) ->
  (Int, [Int]) ->
  B.ByteString m r ->
  m ([Text], [[a]])
parseCols h n c p (nc, cs) bs =
  case h of
    HasHeader -> do
      (eHeaders, bs') <- bs & S.parse (cols field nc cs c)
      case eHeaders of
        Right _ -> ([], []) & pure
        Left heads -> do
          rs <- parseCsvBody_ n c (cols p nc cs) bs'
          (decodeUtf8 <$> heads, rs) & pure
    NoHeader -> do
      rs <- parseCsvBody_ n c (cols p nc cs) bs
      ([], rs) & pure
