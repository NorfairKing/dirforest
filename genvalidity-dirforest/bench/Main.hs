{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.DirForest
import Data.GenValidity.Criterion
import Data.GenValidity.DirForest ()
import Data.Word

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @(DirTree Word8),
      genValidBench @(DirForest Word8)
    ]
