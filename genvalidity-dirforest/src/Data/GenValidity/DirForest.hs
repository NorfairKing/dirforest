{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.DirForest where

import Data.DirForest (DirForest (..), DirTree (..))
import qualified Data.DirForest as DF
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import qualified Data.Map as M
import Path
import System.FilePath as FP
import Test.QuickCheck

instance (Ord a, GenValid a) => GenValid (DirForest a) where
  shrinkValid = shrinkValidStructurally
  genValid = DirForest . M.fromList <$> genListOf genPathValuePair
    where
      genPathValuePair =
        oneof
          [ (,) <$> (fromRelFile <$> genValid) <*> (NodeFile <$> genValid),
            (,) <$> (FP.dropTrailingPathSeparator . fromRelDir <$> genValid) <*> (NodeDir <$> genNonEmptyDirForest)
          ]
      genNonEmptyDirForest = do
        df <- genValid
        ((,) <$> genValid <*> genValid)
          `suchThatMap` ( \(p, cts) -> case DF.insert p cts df of
                            Left _ -> Nothing
                            Right r -> Just r
                        )

instance (Ord a, GenValid a) => GenValid (DirTree a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

changedDirForest :: (Ord a, GenValid a) => DirForest a -> Gen (DirForest a)
changedDirForest = traverse (\v -> genValid `suchThat` (/= v))

disjunctDirForest :: (Ord a, GenValid a) => DirForest a -> Gen (DirForest a)
disjunctDirForest m = genValid `suchThat` (DF.null . DF.intersection m)
