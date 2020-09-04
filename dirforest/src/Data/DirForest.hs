{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.DirForest
  ( -- * Dirforest types
    DirTree (..),
    DirForest (..),
    InsertionError (..),
    FOD (..),

    -- * Comparisons
    eq1DirTree,
    ord1DirTree,
    eq1DirForest,
    ord1DirForest,

    -- * Query
    null,
    nullFiles,
    lookup,

    -- * Construction
    empty,
    singletonFile,
    singletonDir,
    insertFile,
    insertDir,

    -- * Pruning
    pruneEmptyDirs,
    anyEmptyDir,

    -- * Conversion

    -- ** Map
    fromFileMap,
    toFileMap,
    fromMap,
    toMap,

    -- ** List
    fromFileList,
    toFileList,

    -- * IO

    -- ** Read
    read,
    readNonHidden,
    readFiltered,
    readNonHiddenFiltered,
    readOneLevel,
    readOneLevelNonHidden,
    readOneLevelFiltered,
    readOneLevelNonHiddenFiltered,
    hiddenRel,

    -- ** Write
    write,

    -- * Combinations

    -- ** Union
    union,
    unionWith,
    unionWithKey,
    unions,

    -- ** Intersection
    intersection,
    intersectionWith,
    intersectionWithKey,
    intersections,

    -- ** Difference
    difference,
    differenceWith,
    differenceWithKey,

    -- * Filter
    filter,
    filterHidden,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Functor.Classes
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Map
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Path.IO
import Path.Internal
import qualified System.FilePath as FP
import Prelude hiding (filter, lookup, null, read)
import qualified Prelude

data DirTree a
  = NodeFile a
  | NodeDir (DirForest a)
  deriving (Show, Generic, Functor)

instance (Validity a, Ord a) => Validity (DirTree a)

instance Eq a => Eq (DirTree a) where
  (==) = eq1DirTree (==)

instance Ord a => Ord (DirTree a) where
  compare = ord1DirTree compare

instance Eq1 DirTree where
  liftEq = eq1DirTree

instance Ord1 DirTree where
  liftCompare = ord1DirTree

instance NFData a => NFData (DirTree a)

instance Foldable DirTree where
  foldMap func =
    \case
      NodeFile v -> func v
      NodeDir df -> foldMap func df

instance Traversable DirTree where
  traverse func =
    \case
      NodeFile v -> NodeFile <$> func v
      NodeDir df -> NodeDir <$> traverse func df

instance FromJSON a => FromJSON (DirTree a) where
  parseJSON v = NodeFile <$> parseJSON v <|> NodeDir <$> parseJSON v

instance ToJSON a => ToJSON (DirTree a) where
  toJSON = \case
    NodeFile v -> toJSON v
    NodeDir df -> toJSON df

eq1DirTree :: (a -> b -> Bool) -> DirTree a -> DirTree b -> Bool
eq1DirTree eq dt1 dt2 = case (dt1, dt2) of
  (NodeFile a1, NodeFile a2) -> eq a1 a2
  (NodeDir df1, NodeDir df2) -> eq1DirForest eq df1 df2
  _ -> False

ord1DirTree :: (a -> b -> Ordering) -> DirTree a -> DirTree b -> Ordering
ord1DirTree cmp dt1 dt2 = case (dt1, dt2) of
  (NodeFile a1, NodeFile a2) -> cmp a1 a2
  (NodeDir df1, NodeDir df2) -> ord1DirForest cmp df1 df2
  (NodeFile _, NodeDir _) -> LT
  (NodeDir _, NodeFile _) -> GT

newtype DirForest a
  = DirForest
      { unDirForest :: Map FilePath (DirTree a)
      }
  deriving (Show, Generic, Functor)

instance (Validity a, Ord a) => Validity (DirForest a) where
  validate df@(DirForest m) =
    mconcat
      [ genericValidate df,
        decorateMap m $ \p dt ->
          let isTopLevel p_ = parent p_ == [reldir|./|]
           in case dt of
                NodeFile _ ->
                  let rf = Path p :: Path Rel File
                   in mconcat
                        [ declare "There are no separators on this level" $ isTopLevel rf,
                          validate (Path p :: Path Rel File)
                        ]
                NodeDir (DirForest _) ->
                  let rd = Path (FP.addTrailingPathSeparator p) :: Path Rel Dir
                   in mconcat
                        [ declare "the path has no trailing path separator"
                            $ not
                            $ FP.hasTrailingPathSeparator p,
                          declare "There are no separators on this level" $ isTopLevel rd, -- We need this for equality with the files.
                          validate rd
                        ]
      ]

instance Eq a => Eq (DirForest a) where
  (==) = eq1DirForest (==)

instance Ord a => Ord (DirForest a) where
  compare = ord1DirForest compare

instance Eq1 DirForest where
  liftEq = eq1DirForest

instance Ord1 DirForest where
  liftCompare = ord1DirForest

instance NFData a => NFData (DirForest a)

instance Semigroup (DirForest a) where
  (<>) = union

instance Monoid (DirForest a) where
  mempty = empty
  mappend = (<>)

instance Foldable DirForest where
  foldMap func (DirForest dtm) = foldMap (foldMap func) dtm

instance Traversable DirForest where
  traverse func (DirForest dtm) = DirForest <$> traverse (traverse func) dtm

instance ToJSON a => ToJSON (DirForest a) where
  toJSON = toJSON . unDirForest

instance FromJSON a => FromJSON (DirForest a) where
  parseJSON = fmap DirForest . parseJSON

eq1DirForest :: (a -> b -> Bool) -> DirForest a -> DirForest b -> Bool
eq1DirForest eq (DirForest m1) (DirForest m2) =
  let l1 = M.toAscList m1
      l2 = M.toAscList m2
   in length l1 == length l2 && liftEq (\(p1, a1) (p2, a2) -> p1 == p2 && eq1DirTree eq a1 a2) l1 l2

ord1DirForest :: (a -> b -> Ordering) -> DirForest a -> DirForest b -> Ordering
ord1DirForest cmp (DirForest m1) (DirForest m2) =
  let l1 = M.toAscList m1
      l2 = M.toAscList m2
   in liftCompare (\(p1, a1) (p2, a2) -> compare p1 p2 <> ord1DirTree cmp a1 a2) l1 l2

-- | File or Dir
data FOD a
  = F a
  | D
  deriving (Show, Eq, Ord, Generic, Functor)

instance Validity a => Validity (FOD a)

-- | The empty forest
empty :: DirForest a
empty = DirForest M.empty

-- | True iff the forest is entirely empty
null :: DirForest a -> Bool
null (DirForest dtm) = M.null dtm

-- | True iff there are only empty directories in the directory forest
nullFiles :: DirForest a -> Bool
nullFiles (DirForest df) = all goTree df
  where
    goTree = \case
      NodeFile _ -> False
      NodeDir df' -> nullFiles df'

singletonFile :: Ord a => Path Rel File -> a -> DirForest a
singletonFile rp a =
  case insertFile rp a empty of
    Right df -> df
    _ -> error "There can't have been anything in the way in an empty dir forest."

singletonDir :: Ord a => Path Rel Dir -> DirForest a
singletonDir rp =
  case insertDir rp empty of
    Right df -> df
    _ -> error "There can't have been anything in the way in an empty dir forest."

-- | Remove all empty directories from a 'DirForest'
--
-- This will return 'Nothing' if the root was also empty.
pruneEmptyDirs :: DirForest a -> Maybe (DirForest a)
pruneEmptyDirs (DirForest m) =
  let m' = M.mapMaybe goTree m
   in if M.null m' then Nothing else Just (DirForest m')
  where
    goTree :: DirTree a -> Maybe (DirTree a)
    goTree dt = case dt of
      NodeFile _ -> Just dt
      NodeDir df -> NodeDir <$> pruneEmptyDirs df

anyEmptyDir :: DirForest a -> Bool
anyEmptyDir (DirForest m) = M.null m || any goTree m
  where
    goTree :: DirTree a -> Bool
    goTree = \case
      NodeFile _ -> False
      NodeDir df -> anyEmptyDir df

lookup ::
  forall a.
  Ord a =>
  Path Rel File ->
  DirForest a ->
  Maybe a
lookup rp df = go df (FP.splitDirectories $ fromRelFile rp)
  where
    go :: DirForest a -> [FilePath] -> Maybe a
    go (DirForest ts) =
      \case
        [] -> Nothing
        [f] -> do
          dt <- M.lookup f ts
          case dt of
            NodeFile contents -> Just contents
            _ -> Nothing
        (d : ds) -> do
          dt <- M.lookup d ts
          case dt of
            NodeDir dt_ -> go dt_ ds
            _ -> Nothing

insertFOD ::
  forall a.
  Ord a =>
  FilePath ->
  FOD a ->
  DirForest a ->
  Either (InsertionError a) (DirForest a)
insertFOD fp fod dirForest = go [reldir|./|] dirForest (FP.splitDirectories fp)
  where
    node = case fod of
      F a -> NodeFile a
      D -> NodeDir empty
    go ::
      Path Rel Dir ->
      DirForest a ->
      [FilePath] ->
      Either (InsertionError a) (DirForest a)
    go cur df@(DirForest ts) =
      \case
        [] -> Right df -- Should not happen, but just insert nothing if it does.
        [f] ->
          -- The last piece
          case M.lookup f ts of
            Nothing ->
              pure $ DirForest $ M.insert f node ts
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile f)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> case fod of
                  F _ -> do
                    let rd = cur </> fromJust (parseRelDir f)
                    Left (DirInTheWay rd df')
                  D -> pure df -- If it's already there, nothing changes

        -- Not the last piece, must be a dir
        (d : ds) ->
          -- Check if this piece is already in the forest
          case M.lookup d ts of
            -- If it isn't, then we need to make it and try again
            Nothing -> do
              let df' = DirForest $ M.insert d (NodeDir empty) ts
              go cur df' (d : ds)
            -- If it is, then we can recurse down there.
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile d)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> do
                  let newCur = cur </> fromJust (parseRelDir d)
                  df'' <- go newCur df' ds
                  pure $ DirForest $ M.insert d (NodeDir df'') ts

insertFile ::
  forall a.
  Ord a =>
  Path Rel File ->
  a ->
  DirForest a ->
  Either (InsertionError a) (DirForest a)
insertFile rp a = insertFOD (fromRelFile rp) (F a)

insertDir ::
  forall a.
  Ord a =>
  Path Rel Dir ->
  DirForest a ->
  Either (InsertionError a) (DirForest a)
insertDir rp = insertFOD (FP.dropTrailingPathSeparator $ fromRelDir rp) D

fromFileList :: Ord a => [(Path Rel File, a)] -> Either (InsertionError a) (DirForest a)
fromFileList = foldM (flip $ uncurry insertFile) empty

toFileList :: Ord a => DirForest a -> [(Path Rel File, a)]
toFileList = M.toList . toFileMap

-- Left-biased
union :: DirForest a -> DirForest a -> DirForest a
union = unionWith const

-- Left-biased
unionWith :: (a -> a -> a) -> DirForest a -> DirForest a -> DirForest a
unionWith func = unionWithKey (\_ a b -> func a b)

-- Left-biased on same paths
-- TODO: maybe we want to make this more general?
unionWithKey :: forall a. (Path Rel File -> a -> a -> a) -> DirForest a -> DirForest a -> DirForest a
unionWithKey func = goForest "" -- Because "" FP.</> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> DirForest a -> DirForest a
    goForest base (DirForest dtm1) (DirForest dtm2) = DirForest $ M.unionWithKey (\p m1 m2 -> goTree (base FP.</> p) m1 m2) dtm1 dtm2
    goTree :: FilePath -> DirTree a -> DirTree a -> DirTree a
    goTree base dt1 dt2 = case (dt1, dt2) of
      (NodeDir df1, NodeDir df2) -> NodeDir $ goForest base df1 df2
      (NodeFile a1, NodeFile a2) -> NodeFile $ func (fromJust $ parseRelFile base) a1 a2
      (l, _) -> l

unions :: [DirForest a] -> DirForest a
unions = foldl' union empty

intersection :: DirForest a -> DirForest b -> DirForest a
intersection = intersectionWith const

intersectionWith :: (a -> b -> c) -> DirForest a -> DirForest b -> DirForest c
intersectionWith func = intersectionWithKey (\_ a b -> func a b)

intersectionWithKey :: forall a b c. (Path Rel File -> a -> b -> c) -> DirForest a -> DirForest b -> DirForest c
intersectionWithKey func = goForest "" -- Because "" FP.</> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> DirForest b -> DirForest c
    goForest base (DirForest dtm1) (DirForest dtm2) =
      DirForest $ M.mapMaybe id $ M.intersectionWithKey (\p m1 m2 -> goTree (base FP.</> p) m1 m2) dtm1 dtm2
    goTree :: FilePath -> DirTree a -> DirTree b -> Maybe (DirTree c)
    goTree base dt1 dt2 = case (dt1, dt2) of
      (NodeDir df1_, NodeDir df2_) -> Just $ NodeDir $ goForest base df1_ df2_
      (NodeFile f1, NodeFile f2) -> Just $ NodeFile $ func (fromJust $ parseRelFile base) f1 f2 -- TODO is this what we want?
      _ -> Nothing

intersections :: [DirForest a] -> DirForest a
intersections = foldl' intersection empty

filter :: Show a => (a -> Bool) -> DirForest a -> DirForest a
filter func = filterWithKey (const func)

filterWithKey :: forall a. (Path Rel File -> a -> Bool) -> DirForest a -> DirForest a
filterWithKey filePred = goForest "" -- Because "" FP.</> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> DirForest a
    goForest base (DirForest df) =
      DirForest $
        M.mapMaybeWithKey
          (\p dt -> goTree (base FP.</> p) dt)
          df
    goTree :: FilePath -> DirTree a -> Maybe (DirTree a) -- Nothing means it will be removed
    goTree base dt = case dt of
      NodeFile cts -> do
        rf <- parseRelFile base
        if filePred rf cts then Just dt else Nothing
      NodeDir df -> Just $ NodeDir $ goForest base df

filterHidden :: forall a. DirForest a -> DirForest a
filterHidden = goForest
  where
    goPair :: FilePath -> DirTree a -> Maybe (DirTree a)
    goPair fp dt = if hiddenHere fp then Nothing else Just $ goTree dt
    goForest :: DirForest a -> DirForest a
    goForest (DirForest m) =
      DirForest $ M.mapMaybeWithKey goPair m
    goTree :: DirTree a -> DirTree a
    goTree dt = case dt of
      NodeFile _ -> dt
      NodeDir df -> NodeDir $ goForest df

difference :: DirForest a -> DirForest b -> DirForest a
difference = differenceWith $ \_ _ -> Nothing

differenceWith :: (a -> b -> Maybe a) -> DirForest a -> DirForest b -> DirForest a
differenceWith func = differenceWithKey $ const func

differenceWithKey :: forall a b. (Path Rel File -> a -> b -> Maybe a) -> DirForest a -> DirForest b -> DirForest a
differenceWithKey func = goForest "" -- Because "" </> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> DirForest b -> DirForest a
    goForest base (DirForest df1_) (DirForest df2_) =
      DirForest $ M.differenceWithKey (\p dt1 dt2 -> goTree (base FP.</> p) dt1 dt2) df1_ df2_
    goTree :: FilePath -> DirTree a -> DirTree b -> Maybe (DirTree a)
    goTree base dt1 dt2 = case (dt1, dt2) of
      (NodeFile v1, NodeFile v2) -> NodeFile <$> func (fromJust $ parseRelFile base) v1 v2
      (NodeFile v, NodeDir _) -> Just $ NodeFile v -- TODO not sure what the semantics are here
      (NodeDir df, NodeFile _) -> Just $ NodeDir df -- TODO not sure what the semantics are here
      (NodeDir df1_, NodeDir df2_) -> Just $ NodeDir $ goForest base df1_ df2_

data InsertionError a
  = FileInTheWay (Path Rel File) a
  | DirInTheWay (Path Rel Dir) (DirForest a)
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (InsertionError a)

fromFileMap :: Ord a => Map (Path Rel File) a -> Either (InsertionError a) (DirForest a)
fromFileMap = foldM (\df (rf, cts) -> insertFile rf cts df) empty . M.toList

toFileMap :: DirForest a -> Map (Path Rel File) a
toFileMap = M.foldlWithKey go M.empty . unDirForest
  where
    go :: Map (Path Rel File) a -> FilePath -> DirTree a -> Map (Path Rel File) a
    go m path =
      \case
        NodeFile contents ->
          let rf = fromJust (parseRelFile path) -- Cannot fail if the original dirforest is valid
           in M.insert rf contents m
        NodeDir df ->
          let rd = fromJust (parseRelDir path) -- Cannot fail if the original dirforest is valid
           in M.union m $ M.mapKeys (rd </>) (toFileMap df)

fromMap :: Ord a => Map FilePath (FOD a) -> Either (InsertionError a) (DirForest a)
fromMap = foldM (\df (rf, fod) -> insertFOD rf fod df) empty . M.toList

toMap :: DirForest a -> Map FilePath (FOD a)
toMap = M.foldlWithKey go M.empty . unDirForest
  where
    go :: Map FilePath (FOD a) -> FilePath -> DirTree a -> Map FilePath (FOD a)
    go m path =
      \case
        NodeFile contents ->
          M.insert path (F contents) m
        NodeDir df ->
          M.insert path D $ M.union m $ M.mapKeys (path FP.</>) (toMap df)

read ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
read = readFiltered (const True) (const True)

readNonHidden ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readNonHidden = readNonHiddenFiltered (const True) (const True)

readNonHiddenFiltered ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  (Path b File -> Bool) ->
  (Path b Dir -> Bool) ->
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readNonHiddenFiltered filePred dirPred root = readFiltered (\f -> go f && filePred f) (\d -> go d && dirPred d) root
  where
    go af = case stripProperPrefix root af of
      Nothing -> True -- Whatever
      Just rf -> not $ hiddenRel rf

readFiltered ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  (Path b File -> Bool) ->
  (Path b Dir -> Bool) ->
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readFiltered filePred dirPred root readFunc = do
  e <- doesDirExist root
  if e
    then walkDirAccumRel (Just decendHandler) outputWriter root
    else pure empty
  where
    decendHandler :: Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (WalkAction Rel)
    decendHandler subdir dirs _ = do
      let toExclude = Prelude.filter (not . dirPred . ((root </> subdir) </>)) dirs
      pure $ WalkExclude toExclude
    outputWriter :: Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (DirForest a)
    outputWriter subdir dirs files = do
      df1 <- foldM goDir empty dirs
      foldM goFile df1 files
      where
        goDir :: DirForest a -> Path Rel Dir -> m (DirForest a)
        goDir df p =
          let path = root </> subdir </> p
           in if dirPred path
                then case insertDir (subdir </> p) df of
                  Left _ ->
                    error
                      "There can't have been anything in the way while reading a dirforest, but there was."
                  Right df' -> pure df'
                else pure df
        goFile :: DirForest a -> Path Rel File -> m (DirForest a)
        goFile df p =
          let path = root </> subdir </> p
           in if filePred path
                then do
                  contents <- readFunc path
                  case insertFile (subdir </> p) contents df of
                    Left _ ->
                      error
                        "There can't have been anything in the way while reading a dirforest, but there was."
                    Right df' -> pure df'
                else pure df

readOneLevel ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readOneLevel = readOneLevelFiltered (const True) (const True)

readOneLevelNonHidden ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readOneLevelNonHidden = readOneLevelNonHiddenFiltered (const True) (const True)

readOneLevelNonHiddenFiltered ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  (Path b File -> Bool) ->
  (Path b Dir -> Bool) ->
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readOneLevelNonHiddenFiltered filePred dirPred root = readOneLevelFiltered (\f -> go f && filePred f) (\d -> go d && dirPred d) root
  where
    go af = case stripProperPrefix root af of
      Nothing -> True -- Whatever
      Just rf -> not $ hiddenRel rf

readOneLevelFiltered ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  (Path b File -> Bool) ->
  (Path b Dir -> Bool) ->
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readOneLevelFiltered filePred dirPred root readFunc = do
  (dirs, files) <- fmap (fromMaybe ([], [])) $ liftIO $ forgivingAbsence $ listDirRel root
  df1 <- foldM goDir empty dirs
  foldM goFile df1 files
  where
    goDir :: DirForest a -> Path Rel Dir -> m (DirForest a)
    goDir df p =
      let path = root </> p
       in if dirPred path
            then case insertDir p df of
              Left _ ->
                error
                  "There can't have been anything in the way while reading a dirforest, but there was."
              Right df' -> pure df'
            else pure df
    goFile :: DirForest a -> Path Rel File -> m (DirForest a)
    goFile df p =
      let path = root </> p
       in if filePred path
            then do
              contents <- readFunc path
              case insertFile p contents df of
                Left _ ->
                  error
                    "There can't have been anything in the way while reading a dirforest, but there was."
                Right df' -> pure df'
            else pure df

write ::
  forall a b.
  (Show a, Ord a) =>
  Path b Dir ->
  DirForest a ->
  (Path b File -> a -> IO ()) ->
  IO ()
write root dirForest writeFunc = do
  ensureDir root
  forM_ (M.toList $ unDirForest dirForest) $ \(path, dt) ->
    case dt of
      NodeFile contents -> do
        f <- parseRelFile path
        let af = root </> f
        writeFunc af contents
      NodeDir df' -> do
        d <- parseRelDir path
        let ad = root </> d
        write ad df' writeFunc

hiddenRel :: Path Rel t -> Bool
hiddenRel = any hiddenHere . FP.splitDirectories . toFilePath

hiddenHere :: FilePath -> Bool
hiddenHere [] = False -- Technically not possible, but fine
hiddenHere ('.' : _) = True
hiddenHere _ = False
