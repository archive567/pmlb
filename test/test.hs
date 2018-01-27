{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.DocTest

main :: IO ()
main = do
  doctest
    [ "app/app.hs"
    , "src/PMLB.hs"
    , "src/PMLB/TH.hs"
    , "src/PMLB/Csv.hs"
    ]
  defaultMain tests

tests :: TestTree
tests =
    testGroup ""
    [
    ]
