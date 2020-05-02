{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.DirForestSpec where

import qualified Data.ByteString as SB
import Data.DirForest (DirForest (..), DirTree (..), InsertionError (..))
import qualified Data.DirForest as DF
import Data.GenValidity.ByteString ()
import Data.GenValidity.DirForest
import Data.List (foldl')
import qualified Data.Map as M
import Data.Map (Map)
import Data.Word
import Path
import Path.IO
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = modifyMaxShrinks (const 100) $ do
  genValidSpec @(DirTree Word8)
  jsonSpecOnValid @(DirTree Word8)
  genValidSpec @(DirForest Word8)
  jsonSpecOnValid @(DirForest Word8)
  describe "empty" $ do
    it "is valid" $ shouldBeValid (DF.empty @Word8)
    it "behaves the same as M.empty" $ DF.toMap @Word8 DF.empty `shouldBe` M.empty
  describe "null" $ do
    it
      "produces valid dir forests"
      $ producesValidsOnValids
        (DF.null @Word8)
    it
      "behaves the same as M.null"
      $ forAllValid
      $ \df -> DF.null @Word8 df `shouldBe` M.null (DF.toMap df)
  describe "singleton" $ do
    it "produces valid forests" $
      producesValidsOnValids2 (DF.singleton @Word8)
    it
      "behaves the same as M.singleton"
      $ forAllValid
      $ \rf -> forAllValid $ \cts -> DF.toMap (DF.singleton @Word8 rf cts) `shouldBe` M.singleton rf cts
  describe "lookup" $ do
    it "produces valid values" $
      producesValidsOnValids2 (DF.lookup @Word8)
    it
      "behaves the same as M.lookup"
      $ forAllValid
      $ \rf -> forAllValid $ \df -> DF.lookup @Word8 rf df `shouldBe` M.lookup rf (DF.toMap df)
  describe "insert" $ do
    it "works for this example of a file"
      $ forAllValid
      $ \contents ->
        DF.insert [relfile|foo|] (contents :: Int) DF.empty
          `shouldBe` Right (DirForest (M.singleton "foo" (NodeFile contents)))
    it "works for this example of a file in a dir"
      $ forAllValid
      $ \contents ->
        DF.insert [relfile|foo/bar|] (contents :: Int) DF.empty
          `shouldBe` Right
            ( DirForest
                (M.singleton "foo" (NodeDir (DirForest (M.singleton "bar" (NodeFile contents)))))
            )
    it "works for this example of two files in the same dir"
      $ forAllValid
      $ \contents1 ->
        forAllValid $ \contents2 -> do
          let dt = DF.singleton [relfile|foo/bar1|] (contents1 :: Int)
          DF.insert [relfile|foo/bar2|] contents2 dt
            `shouldBe` Right
              ( DirForest
                  ( M.singleton
                      "foo"
                      ( NodeDir
                          ( DirForest
                              (M.fromList [("bar1", NodeFile contents1), ("bar2", NodeFile contents2)])
                          )
                      )
                  )
              )
    it "works for if there the exact same file is in the way"
      $ forAllValid
      $ \f ->
        forAllValid $ \contents1 ->
          forAllValid $ \contents2 -> do
            let dt = DF.singleton f (contents1 :: Int)
            DF.insert f contents2 dt `shouldBe` Left (FileInTheWay f contents1)
    it "works for this example with a deeper file in the way"
      $ forAllValid
      $ \contents1 ->
        forAllValid $ \contents2 -> do
          let dt = DF.singleton [relfile|foo|] (contents1 :: Int)
          DF.insert [relfile|foo/bar|] contents2 dt
            `shouldBe` Left (FileInTheWay [relfile|foo|] contents1)
    it "works for this example with a dir in the way"
      $ forAllValid
      $ \contents1 ->
        forAllValid $ \contents2 -> do
          let dt = DF.singleton [relfile|foo/bar|] (contents1 :: Int)
          DF.insert [relfile|foo|] contents2 dt
            `shouldBe` Left (DirInTheWay [reldir|foo|] (DirForest $ M.singleton "bar" (NodeFile contents1)))
    it "works for this example of the same file in two different directories"
      $ forAllValid
      $ \contents1 ->
        forAllValid $ \contents2 -> do
          let df =
                DF.insert [relfile|b/a|] contents2 $
                  DF.singleton [relfile|a|] (contents1 :: Int)
          df
            `shouldBe` Right
              ( DirForest
                  { unDirForest =
                      M.fromList
                        [ ("a", NodeFile contents1),
                          ( "b",
                            NodeDir
                              (DirForest {unDirForest = M.fromList [("a", NodeFile contents2)]})
                          )
                        ]
                  }
              )
    it "produces valid forests" $ producesValidsOnValids3 (DF.insert @Word8)
    it
      "behaves the same as M.lookup when it works"
      $ forAllValid
      $ \rf -> forAllValid $ \cts -> forAllValid $ \df -> case DF.insert @Word8 rf cts df of
        Left _ -> pure ()
        Right df' -> DF.toMap df' `shouldBe` M.insert rf cts (DF.toMap df)
    it "inserts something that can be found again afterward"
      $ forAllValid
      $ \dirForest ->
        forAllValid $ \path ->
          forAllValid $ \contents ->
            case DF.insert path (contents :: Int) dirForest of
              Left _ -> pure () -- Fine.
              Right dirForest' -> DF.lookup path dirForest' `shouldBe` Just contents
  describe "fromList" $ do
    it
      "produces valid dir forests"
      $ producesValidsOnValids
        (DF.fromList @Word8)
    it "behaves the same as M.fromList if it succeeds" $ forAllValid $ \l -> case DF.fromList @Word8 l of
      Left _ -> pure () -- Fine.
      Right df -> DF.toMap df `shouldBe` M.fromList l
  describe "union" $
    do
      it
        "produces valid dir forests"
        $ producesValidsOnValids2
          (DF.union @Word8)
      it "is associative" $
        associativeOnValids (DF.union @Word8)
      it "is commutative" $
        commutativeOnValids (DF.union @Word8)
      it "is idempotent"
        $ forAllValid
        $ \dm1 -> forAllValid $ \dm2 ->
          let res = dm1 `DF.union` dm2
           in (res `DF.union` dm2) `shouldBe` (res :: DirForest Int)
      it "behaves the same as M.union" $ viaMap2 @Word8 DF.union M.union
  describe "unions" $ do
    it
      "produces valid dir forests"
      $ producesValidsOnValids
        (DF.unions @Word8)
    it "behaves the same as M.unions" $ viaMapL @Word8 DF.unions M.unions
  describe "intersection" $ do
    it
      "produces valid dir forests"
      $ producesValidsOnValids2
        (DF.intersection @Word8 @Word8)
    it "is associative" $
      associativeOnValids (DF.intersection @Word8 @Word8)
    it "is commutative" $
      commutativeOnValids (DF.intersection @Word8 @Word8)
    it "is idempotent"
      $ forAllValid
      $ \dm1 -> forAllValid $ \dm2 ->
        let res = (dm1 :: DirForest Word8) `DF.intersection` (dm2 :: DirForest Word8)
         in (res `DF.intersection` dm2) `shouldBe` (res :: DirForest Word8)
    it "should produce an empty list for disjunct dir forests"
      $ forAllValid
      $ \dm1 -> forAll (disjunctDirForest dm1) $ \dm2 -> DF.intersection @Word8 @Word8 dm1 dm2 `shouldBe` DF.empty
    it "shows that any dirforest is its own fixed point" $ forAllValid $ \df -> DF.intersection @Word8 @Word8 df df `shouldBe` df
    it "behaves the same as M.intersection" $ viaMap2 @Word8 DF.intersection M.intersection
  describe "intersections" $ do
    it
      "produces valid dir forests"
      $ producesValidsOnValids
        (DF.intersections @Word8)
    it "behaves the same as M.intersections" $ viaMapL @Word8 DF.intersections (foldl' M.intersection M.empty)
  describe "filter" $ do
    it
      "produces valid dir forests for const True"
      $ producesValidsOnValids
      $ DF.filter
        @Word8
        (const True)
    it
      "produces the same forest for const True"
      $ forAllValid
      $ \df -> DF.filter @Word8 (const True) df `shouldBe` df
    it
      "produces valid dir forests for const False"
      $ producesValidsOnValids
      $ DF.filter @Word8
        (const False)
    it
      "produces the empty forest for const False"
      $ forAllValid
      $ \df -> DF.filter @Word8 (const False) df `shouldBe` DF.empty
    it "behaves the same as M.filter" $ forAllValid $ \(w :: Word8) -> viaMap (DF.filter (>= w)) (M.filter (>= w))
  describe "filter"
    $ it
      "produces valid dir forests for const True"
    $ producesValidsOnValids
      (DF.filterHidden @Word8)
  describe "difference" $ do
    it
      "produces valid dir forests"
      $ producesValidsOnValids2
        (DF.difference @Word8 @Word8)
    it "is associative" $
      associativeOnValids (DF.difference @Word8)
    it "behaves the same as M.difference" $ viaMap2 @Word8 DF.difference M.difference
  describe "fromMap" $ do
    it "is the inverse of toMap if it succeeds starting from a dirforest" $ inverseFunctionsIfSecondSucceedsOnValid DF.toMap (DF.fromMap @Word8)
    it "is the inverse of toMap if it succeeds starting from a map" $ inverseFunctionsIfFirstSucceedsOnValid DF.fromMap (DF.toMap @Word8)
  describe "toMap" $ do
    it "works for this example with a file"
      $ forAllValid
      $ \contents ->
        let df = DF.DirForest (M.singleton "foo" (NodeFile (contents :: Int)))
         in DF.toMap df `shouldBe` M.fromList [([relfile|foo|], contents)]
    it "works for this example with a directory"
      $ forAllValid
      $ \contents ->
        let df =
              DirForest
                ( M.singleton
                    "foo"
                    (NodeDir (DirForest (M.singleton "bar" (NodeFile (contents :: Int)))))
                )
         in DF.toMap df `shouldBe` M.fromList [([relfile|foo/bar|], contents)]
    it "works for this example of two files in the same dir"
      $ forAllValid
      $ \contents1 ->
        forAllValid $ \contents2 -> do
          let df =
                DirForest
                  ( M.singleton
                      "foo"
                      ( NodeDir
                          ( DirForest
                              ( M.fromList
                                  [("bar1", NodeFile contents1), ("bar2", NodeFile (contents2 :: Int))]
                              )
                          )
                      )
                  )
          DF.toMap df
            `shouldBe` M.fromList [([relfile|foo/bar1|], contents1), ([relfile|foo/bar2|], contents2)]
    it "works for this example"
      $ forAllValid
      $ \contents1 ->
        forAllValid $ \contents2 -> do
          let df =
                DirForest
                  ( M.singleton
                      "a"
                      ( NodeDir
                          ( DirForest
                              ( M.fromList
                                  [("a", NodeFile contents1), ("b", NodeFile (contents2 :: Int))]
                              )
                          )
                      )
                  )
          DF.toMap df
            `shouldBe` M.fromList [([relfile|a/a|], contents1), ([relfile|a/b|], contents2)]
    it "produces valid maps" $ producesValidsOnValids (DF.toMap @Word8)
  describe "readDirForest" $ do
    it "reads an empty forest if the directory doesn't exist" $ do
      tdirDeleted <- withSystemTempDir "mergeful-dirtree" pure
      dirForest' <- DF.read tdirDeleted (SB.readFile . fromAbsFile)
      dirForest' `shouldBe` DF.empty
    modifyMaxSuccess (`div` 10)
      $ modifyMaxSize (`div` 2)
      $ do
        it "reads valid forests"
          $ forAllValid
          $ \dirForest ->
            withSystemTempDir "mergeful-dirtree" $ \tdir -> do
              DF.write tdir dirForest $ \p contents -> SB.writeFile (fromAbsFile p) contents
              dirForest' <- DF.read tdir (SB.readFile . fromAbsFile)
              shouldBeValid dirForest'
        it "reads what was written"
          $ forAllValid
          $ \dirForest ->
            withSystemTempDir "mergeful-dirtree" $ \tdir -> do
              DF.write tdir dirForest $ \p contents -> SB.writeFile (fromAbsFile p) contents
              dirForest' <- DF.read tdir (SB.readFile . fromAbsFile)
              dirForest' `shouldBe` dirForest

viaMap :: (Show a, Ord a, GenValid a) => (DirForest a -> DirForest a) -> (Map (Path Rel File) a -> Map (Path Rel File) a) -> Property
viaMap dfFunc mFunc =
  forAllValid $ \df -> DF.toMap (dfFunc df) `shouldBe` mFunc (DF.toMap df)

viaMap2 :: (Show a, Ord a, GenValid a) => (DirForest a -> DirForest a -> DirForest a) -> (Map (Path Rel File) a -> Map (Path Rel File) a -> Map (Path Rel File) a) -> Property
viaMap2 dfFunc mFunc =
  forAllValid $ \df1 -> forAllValid $ \df2 -> DF.toMap (dfFunc df1 df2) `shouldBe` mFunc (DF.toMap df1) (DF.toMap df2)

viaMapL :: (Show a, Ord a, GenValid a) => ([DirForest a] -> DirForest a) -> ([Map (Path Rel File) a] -> Map (Path Rel File) a) -> Property
viaMapL dfFunc mFunc = forAllValid $ \dfs -> DF.toMap (dfFunc dfs) `shouldBe` mFunc (map DF.toMap dfs)
