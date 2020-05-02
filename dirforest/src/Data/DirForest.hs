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
    emptyDirForest,
    singletonDirForest,
    lookupDirForest,
    insertDirForest,
    dirForestFromList,
    dirForestToList,
    unionDirForest,
    unionsDirForest,
    nullDirForest,
    intersectionDirForest,
    filterDirForest,
    filterHiddenDirForest,
    differenceDirForest,
    DirForestInsertionError (..),
    dirForestFromMap,
    dirForestToMap,
    readDirForest,
    readFilteredDirForest,
    writeDirForest,
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
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
import qualified System.FilePath as FP

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
                  case parseRelFile p of
                    Nothing -> invalid $ "cannot parse as a relative directory: " <> p
                    Just rf ->
                      mconcat
                        [ decorate "The can path can be parsed as a valid relative dir path" $
                            mconcat
                              [ declare "and to the same path, no less" $ fromRelFile rf == p,
                                validate rf
                              ],
                          declare "There are no separators on this level" $ isTopLevel rf
                        ]
                NodeDir (DirForest df') ->
                  mconcat
                    [ declare "The contained dirforest is nonempty" $ not $ M.null df',
                      declare "the path has no trailing path separator"
                        $ not
                        $ FP.hasTrailingPathSeparator p,
                      case parseRelDir p of
                        Nothing -> invalid $ "cannot parse as a relative directory: " <> p
                        Just rd ->
                          mconcat
                            [ decorate "The can path can be parsed as a valid relative dir path" $
                                mconcat
                                  [ declare "and to the same path, no less" $
                                      FP.dropTrailingPathSeparator (fromRelDir rd) == p,
                                    validate rd
                                  ],
                              declare "There are no separators on this level" $ isTopLevel rd
                            ]
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

emptyDirForest :: DirForest a
emptyDirForest = DirForest M.empty

singletonDirForest :: Ord a => Path Rel File -> a -> DirForest a
singletonDirForest rp a =
  case insertDirForest rp a emptyDirForest of
    Right df -> df
    _ -> error "There can't have been anything in the way in an empty dir forest."

lookupDirForest ::
  forall a.
  Ord a =>
  Path Rel File ->
  DirForest a ->
  Maybe a
lookupDirForest rp df = go df (FP.splitDirectories $ fromRelFile rp)
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

insertDirForest ::
  forall a.
  Ord a =>
  Path Rel File ->
  a ->
  DirForest a ->
  Either (DirForestInsertionError a) (DirForest a)
insertDirForest rp a df = go [reldir|./|] df (FP.splitDirectories $ fromRelFile rp)
  where
    go ::
      Path Rel Dir ->
      DirForest a ->
      [FilePath] ->
      Either (DirForestInsertionError a) (DirForest a)
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
              pure $ DirForest $ M.insert d (NodeDir $ singletonDirForest rf a) ts
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile d)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> do
                  let newCur = cur </> fromJust (parseRelDir d)
                  df'' <- go newCur df' ds
                  pure $ DirForest $ M.insert d (NodeDir df'') ts

dirForestFromList :: Ord a => [(Path Rel File, a)] -> Either (DirForestInsertionError a) (DirForest a)
dirForestFromList = foldM (flip $ uncurry insertDirForest) emptyDirForest

dirForestToList :: Ord a => DirForest a -> [(Path Rel File, a)]
dirForestToList = M.toList . dirForestToMap

-- Left-biased
unionDirForest :: Ord a => DirForest a -> DirForest a -> DirForest a
unionDirForest = unionDirForestWith const

-- Left-biased
unionDirForestWith :: Ord a => (a -> a -> a) -> DirForest a -> DirForest a -> DirForest a
unionDirForestWith func = unionDirForestWithKey (\_ a b -> func a b)

-- Left-biased on same paths
-- TODO: maybe we want to make this more general?
unionDirForestWithKey :: forall a. Ord a => (Path Rel File -> a -> a -> a) -> DirForest a -> DirForest a -> DirForest a
unionDirForestWithKey func = goForest "" -- Because "" FP.</> "anything" = "anything"
  where
    goForest :: FilePath -> DirForest a -> DirForest a -> DirForest a
    goForest base (DirForest dtm1) (DirForest dtm2) = DirForest $ M.unionWithKey (\p m1 m2 -> goTree (base FP.</> p) m1 m2) dtm1 dtm2
    goTree :: FilePath -> DirTree a -> DirTree a -> DirTree a
    goTree base dt1 dt2 = case (dt1, dt2) of
      (NodeDir df1, NodeDir df2) -> NodeDir $ goForest base df1 df2
      (NodeFile a1, NodeFile a2) -> NodeFile $ func (fromJust $ parseRelFile base) a1 a2
      (l, _) -> l

unionsDirForest :: Ord a => [DirForest a] -> DirForest a
unionsDirForest = foldl' unionDirForest emptyDirForest

nullDirForest :: DirForest a -> Bool
nullDirForest (DirForest dtm) = M.null dtm

intersectionDirForest :: DirForest a -> DirForest b -> DirForest a
intersectionDirForest = intersectionDirForestWith const

intersectionDirForestWith :: (a -> b -> c) -> DirForest a -> DirForest b -> DirForest c
intersectionDirForestWith func = intersectionDirForestWithKey (\_ a b -> func a b)

intersectionDirForestWithKey :: forall a b c. (Path Rel File -> a -> b -> c) -> DirForest a -> DirForest b -> DirForest c
intersectionDirForestWithKey func df1 df2 = fromMaybe emptyDirForest $ goForest "" df1 df2 -- Because "" FP.</> "anything" = "anything"
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

filterDirForest :: forall a. Show a => (Path Rel File -> a -> Bool) -> DirForest a -> DirForest a
filterDirForest filePred = fromMaybe emptyDirForest . goForest "" -- Because "" FP.</> "anything" = "anything"
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

filterHiddenDirForest :: forall a. DirForest a -> DirForest a
filterHiddenDirForest = fromMaybe emptyDirForest . goForest
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

differenceDirForest :: DirForest a -> DirForest b -> DirForest a
differenceDirForest = differenceDirForestWith $ \_ _ -> Nothing

differenceDirForestWith :: (a -> b -> Maybe a) -> DirForest a -> DirForest b -> DirForest a
differenceDirForestWith func = differenceDirForestWithKey $ const func

differenceDirForestWithKey :: forall a b. (Path Rel File -> a -> b -> Maybe a) -> DirForest a -> DirForest b -> DirForest a
differenceDirForestWithKey func df1 df2 = fromMaybe emptyDirForest $ goForest "" df1 df2 -- Because "" </> "anything" = "anything"
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

data DirForestInsertionError a
  = FileInTheWay (Path Rel File) a
  | DirInTheWay (Path Rel Dir) (DirForest a)
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (DirForestInsertionError a)

-- TODO we'd like a list of errors, ideally
dirForestFromMap :: Ord a => Map (Path Rel File) a -> Either (DirForestInsertionError a) (DirForest a)
dirForestFromMap = foldM (\df (rf, cts) -> insertDirForest rf cts df) emptyDirForest . M.toList

dirForestToMap :: DirForest a -> Map (Path Rel File) a
dirForestToMap = M.foldlWithKey go M.empty . unDirForest
  where
    go :: Map (Path Rel File) a -> FilePath -> DirTree a -> Map (Path Rel File) a
    go m path =
      \case
        NodeFile contents ->
          let rf = fromJust (parseRelFile path) -- Cannot fail if the original dirforest is valid
           in M.insert rf contents m
        NodeDir df ->
          let rd = fromJust (parseRelDir path) -- Cannot fail if the original dirforest is valid
           in M.union m $ M.mapKeys (rd </>) (dirForestToMap df)

readDirForest ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readDirForest = readFilteredDirForest (const True)

readFilteredDirForest ::
  forall a b m.
  (Show a, Ord a, MonadIO m) =>
  (Path b File -> Bool) ->
  Path b Dir ->
  (Path b File -> m a) ->
  m (DirForest a)
readFilteredDirForest filePred root readFunc = do
  mFiles <- liftIO $ forgivingAbsence $ snd <$> listDirRecurRel root
  foldM go emptyDirForest $ fromMaybe [] mFiles
  where
    go df p =
      let path = root </> p
       in if filePred path
            then do
              contents <- readFunc path
              case insertDirForest p contents df of
                Left _ ->
                  error
                    "There can't have been anything in the way while reading a dirforest, but there was."
                Right df' -> pure df'
            else pure df

writeDirForest ::
  forall a b.
  Ord a =>
  Path b Dir ->
  DirForest a ->
  (Path b File -> a -> IO ()) ->
  IO ()
writeDirForest root dirForest writeFunc =
  forM_ (M.toList $ dirForestToMap dirForest) $ \(path, contents) -> do
    let f = root </> path
    ensureDir $ parent f
    writeFunc f contents
