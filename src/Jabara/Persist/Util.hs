{-# LANGUAGE FlexibleContexts #-}
module Jabara.Persist.Util (
    toRecord
    , toKey
    , toTuple
    , toMap
    , getFromMap
    , dummyKey
) where

import Prelude hiding (lookup)
import Database.Persist.Sql
import Data.Maybe (fromJust)
import Data.Map (Map, fromList, lookup)

toRecord :: (Entity record) -> record
toRecord (Entity _ rec) = rec

toKey :: (Entity record) -> Key record
toKey (Entity key _) = key

toTuple :: (Entity record) -> (Key record, record)
toTuple (Entity key record) = (key, record)

toMap :: Ord (Key entity) => [Entity entity] -> Map (Key entity) entity
toMap = fromList . map toTuple

getFromMap :: Ord (Key k) => Key k -> Map (Key k) e -> e
getFromMap key db = fromJust $ lookup key db

dummyKey :: ToBackendKey SqlBackend record => Key record
dummyKey = toSqlKey (-1)
