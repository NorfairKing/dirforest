{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Data.DirForestSpec where

import qualified Data.ByteString as SB
import Data.DirForest (DirForest (..), DirTree (..), InsertionError (..))
import qualified Data.DirForest as DF
import Data.GenValidity.ByteString ()
import Data.GenValidity.DirForest
import qualified Data.Map as M
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
  describe "empty" $ it "is valid" $ shouldBeValid (DF.empty @Word8)
  describe "singleton"
    $ it "produces valid forests"
    $ producesValidsOnValids2 (DF.singleton @Word8)
  describe "lookup"
    $ it "produces valid values"
    $ producesValidsOnValids2 (DF.lookup @Word8)
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
    it "inserts something that can be found again afterward"
      $ forAllValid
      $ \dirForest ->
        forAllValid $ \path ->
          forAllValid $ \contents ->
            case DF.insert path (contents :: Int) dirForest of
              Left _ -> pure () -- Fine.
              Right dirForest' -> DF.lookup path dirForest' `shouldBe` Just contents
  describe "fromList"
    $ it
      "produces valid dir forests"
    $ producesValidsOnValids
      (DF.fromList @Word8)
  describe "union" $
    do
      it
        "produces valid dir forests"
        $ producesValidsOnValids2
          (DF.union @Word8)
      it "is associative" $
        associativeOnValids (DF.union @Word8)
      it "is idempotent"
        $ forAllValid
        $ \dm1 -> forAllValid $ \dm2 ->
          let res = dm1 `DF.union` dm2
           in (res `DF.union` dm2) `shouldBe` (res :: DirForest Int)
  describe "unionsDirForest"
    $ it
      "produces valid dir forests"
    $ producesValidsOnValids
      (DF.unionsDirForest @Word8)
  describe "null"
    $ it
      "produces valid dir forests"
    $ producesValidsOnValids
      (DF.null @Word8)
  describe "intersection" $ do
    it
      "produces valid dir forests"
      $ producesValidsOnValids2
        (DF.intersection @Word8 @Word8)
    it "is associative" $
      associativeOnValids (DF.intersection @Word8 @Word8)
    it "is idempotent"
      $ forAllValid
      $ \dm1 -> forAllValid $ \dm2 ->
        let res = (dm1 :: DirForest Int) `DF.intersection` (dm2 :: DirForest Int)
         in (res `DF.intersection` dm2) `shouldBe` (res :: DirForest Int)
    it "should produce an empty list for disjunct dir forests"
      $ forAllValid
      $ \dm1 -> forAll (disjunctDirForest dm1) $ \dm2 -> DF.intersection @Word8 @Word8 dm1 dm2 `shouldBe` DF.empty
    it "shows that any dirforest is its own fixed point" $ forAllValid $ \df -> DF.intersection @Word8 @Word8 df df `shouldBe` df
  describe "filter" $ do
    it
      "produces valid dir forests for const True"
      $ producesValidsOnValids
      $ DF.filter
        @Word8
        (const $ const True)
    it
      "produces the same forest for const True"
      $ forAllValid
      $ \df -> DF.filter @Word8 (const $ const True) df `shouldBe` df
    it
      "produces valid dir forests for const False"
      $ producesValidsOnValids
      $ DF.filter @Word8
        (const $ const False)
    it
      "produces the empty forest for const False"
      $ forAllValid
      $ \df -> DF.filter @Word8 (const $ const False) df `shouldBe` DF.empty
    it "other tests" pending
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
    it "other tests" pending
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
