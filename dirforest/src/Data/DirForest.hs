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
  ( DirTree (..),
    DirForest (..),
    empty,
    singleton,
    lookup,
    insert,
    fromList,
    toList,
    union,
    unionsDirForest,
    null,
    intersection,
    filter,
    filterHidden,
    difference,
    InsertionError (..),
    fromMap,
    toMap,
    read,
    readFiltered,
    write,
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

instance (NFData a, Ord a) => NFData (DirTree a)

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
      } -- TODO change 'FilePath' to something more sensible like a FileOrDir, maybe?
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
                NodeDir (DirForest df') ->
                  let rd = Path (FP.addTrailingPathSeparator p) :: Path Rel Dir
                   in mconcat
                        [ declare "The contained dirforest is nonempty" $ not $ M.null df',
                          declare "the path has no trailing path separator"
                            $ not
                            $ FP.hasTrailingPathSeparator p,
                          declare "There are no separators on this level" $ isTopLevel rd, -- We need this for equality with the files.
                          validate rd
                        ]
      ]

instance (NFData a, Ord a) => NFData (DirForest a)

instance Foldable DirForest where
  foldMap func (DirForest dtm) = foldMap (foldMap func) dtm

instance Traversable DirForest where
  traverse func (DirForest dtm) = DirForest <$> traverse (traverse func) dtm

instance ToJSON a => ToJSON (DirForest a) where
  toJSON = toJSON . unDirForest

instance FromJSON a => FromJSON (DirForest a) where
  parseJSON = fmap DirForest . parseJSON

empty :: DirForest a
empty = DirForest M.empty

singleton :: Ord a => Path Rel File -> a -> DirForest a
singleton rp a =
  case insert rp a empty of
    Right df -> df
    _ -> error "There can't have been anything in the way in an empty dir forest."

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
union :: Ord a => DirForest a -> DirForest a -> DirForest a
union = unionWith const

-- Left-biased
unionWith :: Ord a => (a -> a -> a) -> DirForest a -> DirForest a -> DirForest a
unionWith func = unionWithKey (\_ a b -> func a b)

-- Left-biased on same paths
-- TODO: maybe we want to make this more general?
unionWithKey :: forall a. Ord a => (Path Rel File -> a -> a -> a) -> DirForest a -> DirForest a -> DirForest a
unionWithKey func = goForest "" -- Because "" FP.</> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> DirForest a -> DirForest a
    goForest base (DirForest dtm1) (DirForest dtm2) = DirForest $ M.unionWithKey (\p m1 m2 -> goTree (base FP.</> p) m1 m2) dtm1 dtm2
    goTree :: FilePath -> DirTree a -> DirTree a -> DirTree a
    goTree base dt1 dt2 = case (dt1, dt2) of
      (NodeDir df1, NodeDir df2) -> NodeDir $ goForest base df1 df2
      (NodeFile a1, NodeFile a2) -> NodeFile $ func (fromJust $ parseRelFile base) a1 a2
      (l, _) -> l

unionsDirForest :: Ord a => [DirForest a] -> DirForest a
unionsDirForest = foldl' union empty

null :: DirForest a -> Bool
null (DirForest dtm) = M.null dtm

intersection :: DirForest a -> DirForest b -> DirForest a
intersection = intersectionWith const

intersectionWith :: (a -> b -> c) -> DirForest a -> DirForest b -> DirForest c
intersectionWith func = intersectionWithKey (\_ a b -> func a b)

intersectionWithKey :: forall a b c. (Path Rel File -> a -> b -> c) -> DirForest a -> DirForest b -> DirForest c
intersectionWithKey func df1 df2 = fromMaybe empty $ goForest "" df1 df2 -- Because "" FP.</> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> DirForest b -> Maybe (DirForest c)
    goForest base (DirForest dtm1) (DirForest dtm2) =
      let df' = M.mapMaybe id $ M.intersectionWithKey (\p m1 m2 -> goTree (base FP.</> p) m1 m2) dtm1 dtm2
       in if M.null df'
            then Nothing
            else Just $ DirForest df'
    goTree :: FilePath -> DirTree a -> DirTree b -> Maybe (DirTree c)
    goTree base dt1 dt2 = case (dt1, dt2) of
      (NodeDir df1_, NodeDir df2_) -> NodeDir <$> goForest base df1_ df2_
      (NodeFile f1, NodeFile f2) -> Just $ NodeFile $ func (fromJust $ parseRelFile base) f1 f2 -- TODO is this what we want?
      _ -> Nothing

filter :: forall a. Show a => (Path Rel File -> a -> Bool) -> DirForest a -> DirForest a
filter filePred = fromMaybe empty . goForest "" -- Because "" FP.</> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> Maybe (DirForest a)
    goForest base (DirForest df) =
      let df' =
            M.mapMaybeWithKey
              (\p dt -> goTree (base FP.</> p) dt)
              df
       in if M.null df'
            then Nothing
            else Just (DirForest df')
    goTree :: FilePath -> DirTree a -> Maybe (DirTree a) -- Nothing means it will be removed
    goTree base dt = case dt of
      NodeFile cts -> do
        rf <- parseRelFile base
        if filePred rf cts then Just dt else Nothing
      NodeDir df -> NodeDir <$> goForest base df

filterHidden :: forall a. DirForest a -> DirForest a
filterHidden = fromMaybe empty . goForest
  where
    goPair :: FilePath -> DirTree a -> Maybe (DirTree a)
    goPair fp dt = if hidden fp then Nothing else goTree dt
    goForest :: DirForest a -> Maybe (DirForest a)
    goForest (DirForest m) =
      let m' = M.mapMaybeWithKey goPair m
       in if M.null m' then Nothing else Just (DirForest m')
    goTree :: DirTree a -> Maybe (DirTree a)
    goTree dt = case dt of
      NodeFile _ -> Just dt
      NodeDir df -> NodeDir <$> goForest df
    hidden [] = False -- Technically not possible, but fine
    hidden ('.' : _) = True
    hidden _ = False

difference :: DirForest a -> DirForest b -> DirForest a
difference = differenceWith $ \_ _ -> Nothing

differenceWith :: (a -> b -> Maybe a) -> DirForest a -> DirForest b -> DirForest a
differenceWith func = differenceWithKey $ const func

differenceWithKey :: forall a b. (Path Rel File -> a -> b -> Maybe a) -> DirForest a -> DirForest b -> DirForest a
differenceWithKey func df1 df2 = fromMaybe empty $ goForest "" df1 df2 -- Because "" </> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> DirForest b -> Maybe (DirForest a)
    goForest base (DirForest df1_) (DirForest df2_) =
      let df' = M.differenceWithKey (\p dt1 dt2 -> goTree (base FP.</> p) dt1 dt2) df1_ df2_
       in if M.null df' then Nothing else Just $ DirForest df'
    goTree :: FilePath -> DirTree a -> DirTree b -> Maybe (DirTree a)
    goTree base dt1 dt2 = case (dt1, dt2) of
      (NodeFile v1, NodeFile v2) -> NodeFile <$> func (fromJust $ parseRelFile base) v1 v2
      (NodeFile v, NodeDir _) -> Just $ NodeFile v -- TODO not sure what the semantics are here
      (NodeDir df, NodeFile _) -> Just $ NodeDir df -- TODO not sure what the semantics are here
      (NodeDir df1_, NodeDir df2_) -> NodeDir <$> goForest base df1_ df2_

data InsertionError a
  = FileInTheWay (Path Rel File) a
  | DirInTheWay (Path Rel Dir) (DirForest a)
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (InsertionError a)

-- TODO we'd like a list of errors, ideally
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
