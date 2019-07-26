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
import Test.DocTest

main :: IO ()
main =
  doctest
    [ "app/app.lhs"
    , "src/PMLB.hs"
    , "src/PMLB/Csv.hs"
    ]
