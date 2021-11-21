{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.DirForestSpec where

import Control.Monad
import qualified Data.ByteString as SB
import Data.DirForest (DirForest (..), DirTree (..), FOD (..), InsertValidation (..), InsertionError (..))
import qualified Data.DirForest as DF
import Data.Either
import Data.Functor.Identity
import Data.GenValidity.ByteString ()
import Data.GenValidity.DirForest
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word
import Path
import Path.IO
import qualified System.FilePath as FP
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson
import Text.Show.Pretty

spec :: Spec
spec = modifyMaxShrinks (const 1000) $ do
  eqSpec @(DirTree Word8)
  ordSpec @(DirTree Word8)
  eqSpec @(DirForest Word8)
  ordSpec @(DirForest Word8)
  genValidSpec @(DirTree Word8)
  jsonSpec @(DirTree Word8)
  genValidSpec @(DirForest Word8)
  jsonSpec @(DirForest Word8)
  describe "empty" $ do
    it "is valid" $ shouldBeValid (DF.empty @Word8)
    it "behaves the same as M.empty" $ DF.toFileMap @Word8 DF.empty `shouldBe` M.empty
  describe "null" $ do
    it
      "produces valid dir forests"
      $ producesValid
        (DF.null @Word8)
    it
      "behaves the same as M.null"
      $ forAllValid $
        \df -> DF.null @Word8 df `shouldBe` M.null (DF.toMap df)
  describe "singletonFile" $ do
    it "produces valid forests" $
      producesValid2 (DF.singletonFile @Word8)
    it "behaves the same as M.singletonFile" $ forAllValid $ \rf -> forAllValid $ \cts -> DF.toMap (DF.singletonFile @Word8 rf cts) `shouldBe` M.singleton (fromRelFile rf) (F cts)
  describe "singletonDir" $ do
    it "produces valid forests" $
      producesValid (DF.singletonDir @Word8)
    it "behaves the same as M.singletonDir" $ forAllValid $ \rd -> DF.toMap (DF.singletonDir @Word8 rd) `shouldBe` M.singleton (FP.dropTrailingPathSeparator $ fromRelDir rd) D
  describe "mapWithPath" $ do
    it "behaves the same as M.mapWithKey for increments" $
      forAllValid $ \df ->
        let incFod = const $ \case
              F i -> F (i + 1 :: Word8)
              D -> D
            inc1 = const (+ 1)
         in Right (DF.mapWithPath inc1 df) `shouldBe` DF.fromMap (M.mapWithKey incFod (DF.toMap df))
    it "behaves the same as M.mapWithKey for the function that takes the path" $
      forAllValid $ \df ->
        let pathFodF p = \case
              F _ -> F p
              D -> D
            pathF p _ = toFilePath p
         in Right (DF.mapWithPath pathF (df :: DirForest Word8)) `shouldBe` DF.fromMap (M.mapWithKey pathFodF (DF.toMap df))
  describe "traverseWithPath" $ do
    it "behaves the same as M.traverseWithKey for increments" $
      forAllValid $ \df ->
        let incFod _ x =
              Identity $
                case x of
                  F i -> F (i + 1 :: Word8)
                  D -> D
            inc1 _ x = Identity $ x + 1
         in Right <$> DF.traverseWithPath inc1 df `shouldBe` (DF.fromMap <$> M.traverseWithKey incFod (DF.toMap df))
    it "behaves the same as M.traverseWithKey for for the function that takes the path" $
      forAllValid $ \df ->
        let incFod p x =
              Identity $
                case x of
                  F _ -> F p
                  D -> D
            inc1 p _ = Identity $ fromRelFile p
         in Right <$> DF.traverseWithPath inc1 (df :: DirForest Word8) `shouldBe` (DF.fromMap <$> M.traverseWithKey incFod (DF.toMap df))
  describe "pruneEmptyDirectories" $ do
    it "produces valid forests" $
      producesValid (DF.pruneEmptyDirs @Word8)
    it "produces forests without any empty maps recursively" $
      forAllValid $
        \df -> case DF.pruneEmptyDirs @Word8 df of
          Nothing -> True
          Just df' -> not $ DF.anyEmptyDir df'
  describe "anyEmptyDir" $
    it "produces valid bools" $
      producesValid (DF.anyEmptyDir @Word8)
  describe "lookup" $ do
    it "produces valid values" $
      producesValid2 (DF.lookup @Word8)
    it
      "behaves the same as M.lookup"
      $ forAllValid $
        \rf -> forAllValid $ \df -> DF.lookup @Word8 rf df `shouldBe` M.lookup rf (DF.toFileMap df)
  describe "insert" $ do
    it "works for this example of a file" $
      forAllValid $
        \contents ->
          DF.insertFile [relfile|foo|] (contents :: Int) DF.empty
            `shouldBe` Right (DirForest (M.singleton "foo" (NodeFile contents)))
    it "works for this example of a file in a dir" $
      forAllValid $
        \contents ->
          DF.insertFile [relfile|foo/bar|] (contents :: Int) DF.empty
            `shouldBe` Right
              ( DirForest
                  (M.singleton "foo" (NodeDir (DirForest (M.singleton "bar" (NodeFile contents)))))
              )
    it "works for this example of a file in a dir if the dir is already there" $
      forAllValid $
        \contents ->
          DF.insertFile [relfile|foo/bar|] (contents :: Int) (DF.singletonDir [reldir|foo|])
            `shouldBe` Right
              ( DirForest
                  (M.singleton "foo" (NodeDir (DirForest (M.singleton "bar" (NodeFile contents)))))
              )
    it "works for this example of two files in the same dir" $
      forAllValid $
        \contents1 ->
          forAllValid $ \contents2 -> do
            let dt = DF.singletonFile [relfile|foo/bar1|] (contents1 :: Int)
            DF.insertFile [relfile|foo/bar2|] contents2 dt
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
    it "works for if there the exact same file is in the way" $
      forAllValid $
        \f ->
          forAllValid $ \contents1 ->
            forAllValid $ \contents2 -> do
              let dt = DF.singletonFile f (contents1 :: Int)
              DF.insertFile f contents2 dt `shouldBe` Left (FileInTheWay f contents1)
    it "works for this example with a deeper file in the way" $
      forAllValid $
        \contents1 ->
          forAllValid $ \contents2 -> do
            let dt = DF.singletonFile [relfile|foo|] (contents1 :: Int)
            DF.insertFile [relfile|foo/bar|] contents2 dt
              `shouldBe` Left (FileInTheWay [relfile|foo|] contents1)
    it "works for this example with a dir in the way" $
      forAllValid $
        \contents1 ->
          forAllValid $ \contents2 -> do
            let dt = DF.singletonFile [relfile|foo/bar|] (contents1 :: Int)
            DF.insertFile [relfile|foo|] contents2 dt
              `shouldBe` Left (DirInTheWay [reldir|foo|] (DirForest $ M.singleton "bar" (NodeFile contents1)))
    it "works for this example of the same file in two different directories" $
      forAllValid $
        \contents1 ->
          forAllValid $ \contents2 -> do
            let df =
                  DF.insertFile [relfile|b/a|] contents2 $
                    DF.singletonFile [relfile|a|] (contents1 :: Int)
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
    it "produces valid forests" $ producesValid3 (DF.insertFile @Word8)
    it
      "behaves the same as M.lookup when it works"
      $ forAllValid $
        \rf -> forAllValid $ \cts -> forAllValid $ \df -> case DF.insertFile @Word8 rf cts df of
          Left _ -> pure ()
          Right df' -> DF.toFileMap df' `shouldBe` M.insert rf cts (DF.toFileMap df)
    it "inserts something that can be found again afterward" $
      forAllValid $
        \dirForest ->
          forAllValid $ \path ->
            forAllValid $ \contents ->
              case DF.insertFile path (contents :: Int) dirForest of
                Left _ -> pure () -- Fine.
                Right dirForest' -> DF.lookup path dirForest' `shouldBe` Just contents
  describe "fromList" $ do
    it
      "produces valid dir forests"
      $ producesValid
        (DF.fromFileList @Word8)
    it "behaves the same as M.fromList if it succeeds" $
      forAllValid $ \l -> case DF.fromFileList @Word8 l of
        Left _ -> pure () -- Fine.
        Right df -> DF.toFileMap df `shouldBe` M.fromList l
  describe "union" $ do
    it "produces valid dir forests" $
      producesValid2
        (DF.union @Word8)
    it "is associative if it succeeds" $
      forAllValid $ \df1 ->
        forAllValid $ \df2 ->
          forAllValid $ \df3 ->
            let el = do
                  df12 <- DF.unpackInsertValidation $ DF.union (df1 :: DirForest Word8) df2
                  DF.unpackInsertValidation $ DF.union df12 df3
                er = do
                  df23 <- DF.unpackInsertValidation $ DF.union df2 df3
                  DF.unpackInsertValidation $ DF.union df1 df23
             in case (,) <$> el <*> er of
                  Left _ -> pure ()
                  Right (l, r) -> l `shouldBe` r
    it "is commutative if it succeeds" $
      forAllValid $ \df1 ->
        forAllValid $ \df2 ->
          let r12 = DF.union (df1 :: DirForest Word8) df2
              r21 = DF.union df2 df1
           in case (,) <$> r12 <*> r21 of
                InsertionErrors _ -> pure ()
                NoInsertionErrors (d1, d2) -> d1 `shouldBe` d2
    it "is idempotent if it succeeds" $
      forAllValid $ \df1 ->
        forAllValid $ \df2 ->
          case (df1 :: DirForest Word8) `DF.union` df2 of
            InsertionErrors _ -> pure ()
            NoInsertionErrors df' -> NoInsertionErrors df' `shouldBe` DF.union df' df2
    it "behaves the same as M.union" $ viaMap2IfSucceeds @Word8 DF.union M.union
    it "Correctly shows an insertion error" $
      let df1' = DirForest $ M.fromList [("b", NodeFile 'b')]
          df1 = DirForest $ M.fromList [("a", NodeDir df1')]
          df2 = DirForest $ M.fromList [("a", NodeFile 'a')]
       in DF.union df1 df2 `shouldBe` InsertionErrors (DirInTheWay [reldir|a|] df1' :| [])
    it "Correctly shows an insertion error the other way around" $
      let df1 = DirForest $ M.fromList [("a", NodeFile 'a')]
          df2' = DirForest $ M.fromList [("b", NodeFile 'b')]
          df2 = DirForest $ M.fromList [("a", NodeDir df2')]
       in DF.union df1 df2 `shouldBe` InsertionErrors (FileInTheWay [relfile|a|] 'a' :| [])
  describe "unions" $ do
    it "produces valid dir forests" $
      producesValid (DF.unions @Word8)
    it "behaves the same as M.unions" $ viaMapLIfSucceeds @Word8 DF.unions M.unions
  describe "intersection" $ do
    it "produces valid dir forests" $
      producesValid2
        (DF.intersection @Word8 @Word8)
    it "is associative" $
      associative (DF.intersection @Word8 @Word8)
    it "is commutative" $
      commutative (DF.intersection @Word8 @Word8)
    it "is idempotent" $
      forAllValid $
        \dm1 -> forAllValid $ \dm2 ->
          let res = (dm1 :: DirForest Word8) `DF.intersection` (dm2 :: DirForest Word8)
           in (res `DF.intersection` dm2) `shouldBe` (res :: DirForest Word8)
    it "should produce an empty list for disjunct dir forests" $
      forAllValid $
        \dm1 -> forAll (disjunctDirForest dm1) $ \dm2 -> DF.intersection @Word8 @Word8 dm1 dm2 `shouldBe` DF.empty
    it "shows that any dirforest is its own fixed point" $ forAllValid $ \df -> DF.intersection @Word8 @Word8 df df `shouldBe` df
    it "behaves the same as M.intersection" $ viaMap2 @Word8 DF.intersection M.intersection
  describe "intersections" $ do
    it "produces valid dir forests" $
      producesValid (DF.intersections @Word8)
    it "behaves the same as M.intersections" $ viaMapL @Word8 DF.intersections (foldl' M.intersection M.empty)
  describe "filter" $ do
    it "produces valid dir forests for const True" $
      producesValid $ DF.filter @Word8 (const True)
    it "produces the same forest for const True" $
      forAllValid $ \df -> DF.filter @Word8 (const True) df `shouldBe` df
    it "produces valid dir forests for const False" $
      producesValid $ DF.filter @Word8 (const False)
    it "produces the empty forest for const False" $
      forAllValid $ \df -> DF.filter @Word8 (const False) df `shouldSatisfy` DF.nullFiles
    it "behaves the same as M.filter" $
      forAllValid $ \(w :: Word8) ->
        viaMap
          (DF.filter (>= w))
          ( M.filter $ \case
              F i -> i >= w
              D -> True
          )
  describe "filter" $
    it "produces valid dir forests for const True" $ producesValid (DF.filterHidden @Word8)
  describe "difference" $ do
    it "produces valid dir forests" $
      producesValid2 (DF.difference @Word8 @Word8)
    it "is associative" $
      associative (DF.difference @Word8)
    it "behaves the same as M.difference" $ viaMap2 @Word8 DF.difference M.difference
  xdescribe "Does not hold because of empty filenames" $
    describe "fromMap" $ do
      it "is the inverse of toMap if it succeeds starting from a dirforest" $ inverseFunctionsIfSecondSucceeds DF.toMap (DF.fromMap @Word8)
      it "is the inverse of toMap if it succeeds starting from a map" $ inverseFunctionsIfFirstSucceeds DF.fromMap (DF.toMap @Word8)
  describe "fromFileMap" $ do
    xdescribe "does not hold because dirs go missing" $ it "is the inverse of toFileMap if it succeeds starting from a dirforest" $ inverseFunctionsIfSecondSucceeds DF.toFileMap (DF.fromFileMap @Word8)
    it "is the inverse of toFileMap if it succeeds starting from a map" $ inverseFunctionsIfFirstSucceeds DF.fromFileMap (DF.toFileMap @Word8)
  describe "toMap" $ do
    it "works for this example with a file" $
      forAllValid $
        \contents ->
          let df = DF.DirForest (M.singleton "foo" (NodeFile (contents :: Int)))
           in DF.toFileMap df `shouldBe` M.fromList [([relfile|foo|], contents)]
    it "works for this example with a directory" $
      forAllValid $
        \contents ->
          let df =
                DirForest
                  ( M.singleton
                      "foo"
                      (NodeDir (DirForest (M.singleton "bar" (NodeFile (contents :: Int)))))
                  )
           in DF.toFileMap df `shouldBe` M.fromList [([relfile|foo/bar|], contents)]
    it "works for this example of two files in the same dir" $
      forAllValid $
        \contents1 ->
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
            DF.toFileMap df
              `shouldBe` M.fromList [([relfile|foo/bar1|], contents1), ([relfile|foo/bar2|], contents2)]
    it "works for this example" $
      forAllValid $
        \contents1 ->
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
            DF.toFileMap df
              `shouldBe` M.fromList [([relfile|a/a|], contents1), ([relfile|a/b|], contents2)]
    it "produces valid maps" $ producesValid (DF.toFileMap @Word8)
  modifyMaxSuccess (`div` 10) $
    modifyMaxSize (`div` 2) $ do
      describe "readDirForest" $ do
        it "reads an empty forest if the directory doesn't exist" $ do
          tdirDeleted <- withSystemTempDir "dirforest-test" pure
          dirForest' <- DF.read tdirDeleted (SB.readFile . fromAbsFile)
          dirForest' `shouldBe` DF.empty
        it "reads valid forests" $
          forAllValid $
            \dirForest ->
              withSystemTempDir "dirforest-test" $ \tdir -> do
                DF.write tdir dirForest $ \p contents -> SB.writeFile (fromAbsFile p) contents
                dirForest' <- DF.read tdir (SB.readFile . fromAbsFile)
                shouldBeValid dirForest'
        let readRoundtrip dirForest =
              withSystemTempDir "dirforest-test" $ \tdir -> do
                DF.write tdir dirForest $ \p contents -> SB.writeFile (fromAbsFile p) contents
                dirForest' <- DF.read tdir (SB.readFile . fromAbsFile)
                dirForest' `shouldBe` dirForest
        it "reads what was written for this simple case with one file" $ forAllValid $ \contents -> readRoundtrip $ DF.singletonFile [relfile|a|] contents
        it "reads what was written for this simple case with one file two directories deep" $ forAllValid $ \contents -> readRoundtrip $ DF.singletonFile [relfile|a/b|] contents
        it "reads what was written for this simple case with one file two directories deep with the same name as the directory" $ forAllValid $ \contents -> readRoundtrip $ DF.singletonFile [relfile|a/a|] contents
        it "reads what was written" $ forAllValid readRoundtrip
      describe "writeDirForest" $
        it "works in a nonexistent root" $
          forAllValid $
            \dirForest -> do
              tdirDeleted <- withSystemTempDir "dirforest-test" pure
              DF.write tdirDeleted dirForest (\p contents -> SB.writeFile (fromAbsFile p) contents)

viaMap :: (Show a, Ord a, GenValid a) => (DirForest a -> DirForest a) -> (Map FilePath (FOD a) -> Map FilePath (FOD a)) -> Property
viaMap dfFunc mFunc =
  forAllValid $ \df -> DF.toMap (dfFunc df) `shouldBe` mFunc (DF.toMap df)

viaMap2 :: (Show a, Ord a, GenValid a) => (DirForest a -> DirForest a -> DirForest a) -> (Map FilePath (FOD a) -> Map FilePath (FOD a) -> Map FilePath (FOD a)) -> Property
viaMap2 dfFunc mFunc =
  forAllValid $ \df1 -> forAllValid $ \df2 -> DF.toMap (dfFunc df1 df2) `shouldBe` mFunc (DF.toMap df1) (DF.toMap df2)

viaMap2IfSucceeds :: (Show a, Ord a, GenValid a) => (DirForest a -> DirForest a -> InsertValidation a (DirForest a)) -> (Map FilePath (FOD a) -> Map FilePath (FOD a) -> Map FilePath (FOD a)) -> Property
viaMap2IfSucceeds dfFunc mFunc =
  forAllValid $ \df1 -> forAllValid $ \df2 ->
    let errOrR = DF.unpackInsertValidation $ dfFunc df1 df2
     in checkCoverage $
          cover 10 (isRight errOrR) "Succeeded" $ case errOrR of
            Left _ -> pure ()
            Right r -> DF.toMap r `shouldBe` mFunc (DF.toMap df1) (DF.toMap df2)

viaMapL :: (Show a, Ord a, GenValid a) => ([DirForest a] -> DirForest a) -> ([Map FilePath (FOD a)] -> Map FilePath (FOD a)) -> Property
viaMapL dfFunc mFunc = forAllValid $ \dfs ->
  let expected = DF.toMap $ dfFunc dfs
      actual = mFunc (map DF.toMap dfs)
   in unless (expected == actual) $
        expectationFailure $
          unlines
            [ "input: ",
              ppShow dfs,
              "expected: ",
              ppShow expected,
              "actual: ",
              ppShow actual
            ]

viaMapLIfSucceeds :: (Show a, Ord a, GenValid a) => ([DirForest a] -> Either e (DirForest a)) -> ([Map FilePath (FOD a)] -> Map FilePath (FOD a)) -> Property
viaMapLIfSucceeds dfFunc mFunc = forAllValid $ \dfs ->
  let errOrR = dfFunc dfs
   in checkCoverage $
        cover 10 (isRight errOrR) "Succeeded" $ case errOrR of
          Left _ -> pure ()
          Right r ->
            let expected = DF.toMap r
                actual = mFunc (map DF.toMap dfs)
             in unless (expected == actual) $
                  expectationFailure $
                    unlines
                      [ "input: ",
                        ppShow dfs,
                        "expected: ",
                        ppShow expected,
                        "actual: ",
                        ppShow actual
                      ]
