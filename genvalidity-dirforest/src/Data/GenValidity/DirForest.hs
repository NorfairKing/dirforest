{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.DirForest where

import Data.DirForest (DirForest (..), DirTree (..), FOD (..))
import qualified Data.DirForest as DF
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import qualified Data.Map as M
import Path
import System.FilePath as FP
import Test.QuickCheck

instance GenValid a => GenValid (FOD a) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance (Ord a, GenValid a) => GenValid (DirForest a) where
  shrinkValid = shrinkValidStructurally
  genValid = DirForest . M.fromList <$> genListOf genPathValuePair
    where
      genPathValuePair = sized $ \s -> do
        (a, b) <- genSplit s
        oneof
          [ (,) <$> resize a (fromRelFile <$> genValid) <*> resize b (NodeFile <$> genValid),
            (,) <$> resize a (FP.dropTrailingPathSeparator . fromRelDir <$> genValid) <*> resize b (NodeDir <$> genValid)
          ]

instance (Ord a, GenValid a) => GenValid (DirTree a) where
  genValid = sized $ \s ->
    oneof
      [ NodeFile <$> resize (max 0 $ s - 1) genValid,
        NodeDir <$> resize (max 0 $ s - 1) genValid
      ]
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

changedDirForest :: (Ord a, GenValid a) => DirForest a -> Gen (DirForest a)
changedDirForest = traverse (\v -> genValid `suchThat` (/= v))

disjunctDirForest :: (Ord a, GenValid a) => DirForest a -> Gen (DirForest a)
disjunctDirForest m = genValid `suchThat` (DF.null . DF.intersection m)
