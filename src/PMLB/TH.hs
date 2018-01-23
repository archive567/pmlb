{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PMLB.TH where

import Flow
import Frames.CSV
import Frames.ColumnTypeable
import Frames.ColumnUniverse
import Frames.Rec
import PMLB (Config, tsep, file)
import Protolude
import qualified Data.Text as Text
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Prelude.Text as PT

-- * Frames.Csv type discovery

-- | template haskell can never grab an example of a type at runtime ...
cfgRowGen :: Config -> RowGen a
cfgRowGen cfg = RowGen [] "" (tsep cfg) "Row" Proxy (PT.readFileLn (file cfg))

tableType' (RowGen [] "" "\t" "Row" Proxy (PT.readFileLn "other/classification/Hill_Valley_with_noise.csv") :: RowGen Columns)

-- | read the first n rows of the csv file TH is looking at
-- >>> import PMLB
-- >>> runProducer def (readRows def 1)
-- [{X1 :-> 39.02, X2 :-> 36.49, X3 :-> 38.2, X4 :-> 38.85, X5 :-> 39.38, X6 :-> 39.74, X7 :-> 37.02, X8 :-> 39.53, X9 :-> 38.81, X10 :-> 38.79, X11 :-> 37.65, X12 :-> 39.34, X13 :-> 38.55, X14 :-> 39.03, X15 :-> 37.21, X16 :-> 36.32, X17 :-> 37.81, X18 :-> 38.95, X19 :-> 36.7, X20 :-> 39.72, X21 :-> 37.06, X22 :-> 37.29, X23 :-> 36.43, X24 :-> 36.53, X25 :-> 36.19, X26 :-> 38.17, X27 :-> 37.3, X28 :-> 36.15, X29 :-> 36.68, X30 :-> 36.7, X31 :-> 36.68, X32 :-> 36.99, X33 :-> 38.92, X34 :-> 37.25, X35 :-> 37.47, X36 :-> 36.32, X37 :-> 35.75, X38 :-> 35.68, X39 :-> 34.66, X40 :-> 34.26, X41 :-> 35.62, X42 :-> 36.6, X43 :-> 34.78, X44 :-> 34.67, X45 :-> 34.3, X46 :-> 33.4, X47 :-> 31.4, X48 :-> 31.75, X49 :-> 31.75, X50 :-> 32.84, X51 :-> 33.76, X52 :-> 35.74, X53 :-> 34.01, X54 :-> 33.91, X55 :-> 36.88, X56 :-> 34.41, X57 :-> 35.52, X58 :-> 36.94, X59 :-> 36.95, X60 :-> 35.57, X61 :-> 38.02, X62 :-> 37.32, X63 :-> 39.05, X64 :-> 37.97, X65 :-> 37.01, X66 :-> 38.98, X67 :-> 38.83, X68 :-> 38.87, X69 :-> 38.03, X70 :-> 38.4, X71 :-> 38.25, X72 :-> 38.61, X73 :-> 36.23, X74 :-> 37.81, X75 :-> 37.98, X76 :-> 38.58, X77 :-> 38.96, X78 :-> 38.97, X79 :-> 39.08, X80 :-> 38.79, X81 :-> 38.79, X82 :-> 36.31, X83 :-> 36.59, X84 :-> 38.19, X85 :-> 37.95, X86 :-> 39.63, X87 :-> 39.27, X88 :-> 37.19, X89 :-> 37.13, X90 :-> 37.47, X91 :-> 37.57, X92 :-> 36.62, X93 :-> 36.92, X94 :-> 38.8, X95 :-> 38.52, X96 :-> 38.07, X97 :-> 36.73, X98 :-> 39.46, X99 :-> 37.5, X100 :-> 39.1, target :-> False}]
--
readRows :: (Monad m) =>
  Config ->
  Int ->
  P.Producer Text m () ->
  m [Row]
readRows cfg n p = p
  P.>-> pipeTableOpt (ParserOptions Nothing (tsep cfg) NoQuoting)
  P.>-> P.take n
  |> P.toListM

readCols :: (Monad m, Monoid a, ColumnTypeable a) => P.Producer Text m () -> m [(Text, a)]
readCols p = p |> readColHeaders (ParserOptions Nothing "\t" NoQuoting)


--  defRows <- withProducer def (\p -> readRows def p P.>-> P.take 5 |> P.toListM :: IO [Row])
--  putStrLn (show defRows :: Text)
