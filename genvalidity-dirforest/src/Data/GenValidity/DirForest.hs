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

instance (GenValid a) => GenValid (FOD a) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance (Ord a, GenValid a) => GenValid (DirForest a) where
  shrinkValid = shrinkValidStructurally
  genValid = genDirForestOf genValid

instance (Ord a, GenValid a) => GenValid (DirTree a) where
  genValid = genDirTreeOf genValid
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genDirForestOf :: (Ord a) => Gen a -> Gen (DirForest a)
genDirForestOf gen = DirForest . M.fromList <$> genListOf genPathValuePair
  where
    genPathValuePair = sized $ \s -> do
      (a, b) <- genSplit s
      oneof
        [ (,)
            <$> resize a (fromRelFile <$> genValid)
            <*> resize b (NodeFile <$> gen),
          (,)
            <$> resize a (FP.dropTrailingPathSeparator . fromRelDir <$> genValid)
            <*> resize b (NodeDir <$> genDirForestOf gen)
        ]

genDirTreeOf :: (Ord a) => Gen a -> Gen (DirTree a)
genDirTreeOf gen =
  sized $ \s ->
    oneof
      [ NodeFile <$> resize (max 0 $ s - 1) gen,
        NodeDir <$> resize (max 0 $ s - 1) (genDirForestOf gen)
      ]

changedDirForest :: (Ord a, GenValid a) => DirForest a -> Gen (DirForest a)
changedDirForest = traverse (\v -> genValid `suchThat` (/= v))

disjunctDirForest :: (Ord a, GenValid a) => DirForest a -> Gen (DirForest a)
disjunctDirForest m = genValid `suchThat` (DF.null . DF.intersection m)
