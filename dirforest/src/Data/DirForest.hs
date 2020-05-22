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

    -- * Query
    null,
    nullFiles,
    lookup,

    -- * Construction
    empty,
    singleton,
    insert,

    -- * Pruning
    pruneEmptyDirs,
    anyEmptyDir,

    -- * Conversion

    -- ** Map
    fromMap,
    toMap,

    -- ** List
    fromList,
    toList,

    -- * IO
    read,
    readFiltered,
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

data DirTree a
  = NodeFile a
  | NodeDir (DirForest a)
  deriving (Show, Eq, Ord, Generic, Functor)

instance (Validity a, Ord a) => Validity (DirTree a)

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

newtype DirForest a
  = DirForest
      { unDirForest :: Map FilePath (DirTree a)
      }
  deriving (Show, Eq, Ord, Generic, Functor)

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

instance NFData a => NFData (DirForest a)

instance Foldable DirForest where
  foldMap func (DirForest dtm) = foldMap (foldMap func) dtm

instance Traversable DirForest where
  traverse func (DirForest dtm) = DirForest <$> traverse (traverse func) dtm

instance ToJSON a => ToJSON (DirForest a) where
  toJSON = toJSON . unDirForest

instance FromJSON a => FromJSON (DirForest a) where
  parseJSON = fmap DirForest . parseJSON

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

singleton :: Ord a => Path Rel File -> a -> DirForest a
singleton rp a =
  case insert rp a empty of
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
anyEmptyDir (DirForest m) = any goTree m
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

insert ::
  forall a.
  Ord a =>
  Path Rel File ->
  a ->
  DirForest a ->
  Either (InsertionError a) (DirForest a)
insert rp a df = go [reldir|./|] df (FP.splitDirectories $ fromRelFile rp)
  where
    go ::
      Path Rel Dir ->
      DirForest a ->
      [FilePath] ->
      Either (InsertionError a) (DirForest a)
    go cur (DirForest ts) =
      \case
        [] -> Right df -- Should not happen, but just insert nothing if it does.
        [f] ->
          case M.lookup f ts of
            Nothing -> pure $ DirForest $ M.insert f (NodeFile a) ts
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile f)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> do
                  let rd = cur </> fromJust (parseRelDir f)
                  Left (DirInTheWay rd df')
        (d : ds) ->
          case M.lookup d ts of
            Nothing -> do
              let rf = fromJust $ parseRelFile $ FP.joinPath ds -- Cannot fail if the original filepath is valid
              pure $ DirForest $ M.insert d (NodeDir $ singleton rf a) ts
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile d)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> do
                  let newCur = cur </> fromJust (parseRelDir d)
                  df'' <- go newCur df' ds
                  pure $ DirForest $ M.insert d (NodeDir df'') ts

fromList :: Ord a => [(Path Rel File, a)] -> Either (InsertionError a) (DirForest a)
fromList = foldM (flip $ uncurry insert) empty

toList :: Ord a => DirForest a -> [(Path Rel File, a)]
toList = M.toList . toMap

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
    goPair fp dt = if hidden fp then Nothing else Just $ goTree dt
    goForest :: DirForest a -> DirForest a
    goForest (DirForest m) =
      DirForest $ M.mapMaybeWithKey goPair m
    goTree :: DirTree a -> DirTree a
    goTree dt = case dt of
      NodeFile _ -> dt
      NodeDir df -> NodeDir $ goForest df
    hidden [] = False -- Technically not possible, but fine
    hidden ('.' : _) = True
    hidden _ = False

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

fromMap :: Ord a => Map (Path Rel File) a -> Either (InsertionError a) (DirForest a)
fromMap = foldM (\df (rf, cts) -> insert rf cts df) empty . M.toList

toMap :: DirForest a -> Map (Path Rel File) a
toMap = M.foldlWithKey go M.empty . unDirForest
  where
    go :: Map (Path Rel File) a -> FilePath -> DirTree a -> Map (Path Rel File) a
    go m path =
      \case
        NodeFile contents ->
          let rf = fromJust (parseRelFile path) -- Cannot fail if the original dirforest is valid
           in M.insert rf contents m
        NodeDir df ->
          let rd = fromJust (parseRelDir path) -- Cannot fail if the original dirforest is valid
           in M.union m $ M.mapKeys (rd </>) (toMap df)

read ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
read = readFiltered (const True)

readFiltered ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  (Path b File -> Bool) ->
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readFiltered filePred root readFunc = do
  mFiles <- liftIO $ forgivingAbsence $ snd <$> listDirRecurRel root
  foldM go empty $ fromMaybe [] mFiles
  where
    go df p =
      let path = root </> p
       in if filePred path
            then do
              contents <- readFunc path
              case insert p contents df of
                Left _ ->
                  error
                    "There can't have been anything in the way while reading a dirforest, but there was."
                Right df' -> pure df'
            else pure df

write ::
  forall a b.
  Ord a =>
  Path b Dir ->
  DirForest a ->
  (Path b File -> a -> IO ()) ->
  IO ()
write root dirForest writeFunc =
  forM_ (M.toList $ toMap dirForest) $ \(path, contents) -> do
    let f = root </> path
    ensureDir $ parent f
    writeFunc f contents
