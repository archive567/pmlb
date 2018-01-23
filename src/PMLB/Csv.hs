module PMLB.Csv where

import Data.Attoparsec.ByteString.Char8 as AC
import Data.Attoparsec.ByteString.Streaming
import Data.ByteString (ByteString)
import Protolude
import qualified Data.ByteString.Char8 as C

-- * low-level generic csv parser helpers

emptyLines :: Parser ()
emptyLines = do
  c <- peekChar
  case c of
    Nothing -> return ()
    Just '\n' -> AC.take 1 *> emptyLines
    Just _ -> return ()
  <?> "emptyLines"

lineEnd :: Parser ()
lineEnd =
  void (AC.char '\n') <|> void (string "\r\n") <|>
  void (AC.char '\r') <?> "end of line"

-- | Most parsing and building routines implicity assume comma separated, and newlines separating rows.
sep :: Char -> Parser ()
sep c = void (AC.char c) <|> lineEnd

extraSeps :: Char -> Parser ()
extraSeps c = void $ many' (sep c)

extraSeps' :: Char -> Parser ()
extraSeps' c = void $ many' (AC.char c <|> AC.char '\r')

unquotedField :: Char -> Parser ByteString
unquotedField c = AC.takeWhile (`C.notElem` (C.singleton c <> "\n\r\"")) <?> "unquoted field"

insideQuotes :: Parser ByteString
insideQuotes =
  C.append <$> AC.takeWhile (/= '"') <*>
  (C.concat <$> many (C.cons <$> dquotes <*> insideQuotes)) <?>
  "inside of double quotes"
  where
    dquotes = string "\"\"" >> return '"' <?> "paired double quotes"

quotedField :: Parser ByteString
quotedField = AC.char '"' *> insideQuotes <* AC.char '"' <?> "quoted field"

field :: Char -> Parser ByteString
field c = quotedField <|> unquotedField c <?> "field"

record :: Char -> Parser [ByteString]
record c = field c `sepBy1` AC.char c

-- | parsers an empty field as a Nothing, non-empty as a Just
maybeField :: Parser a -> Parser (Maybe a)
maybeField p = fmap Just p <|> pure Nothing
