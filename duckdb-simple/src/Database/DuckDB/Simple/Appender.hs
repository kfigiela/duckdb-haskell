{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

{- |
Module      : Database.DuckDB.Simple.Appender
Description : High-performance appender API
-}
module Database.DuckDB.Simple.Appender
  ( withTableAppender
  , withTableAppenderExt
  , withQueryAppender
  , appendTableRow
  , ToAppenderRow(..)
  , tableSchema
  , createTableQuery
  )
  where

import Database.DuckDB.FFI    ( DuckDBAppender,      DuckDBLogicalType,      DuckDBState,      pattern DuckDBSuccess,      pattern DuckDBError,      c_duckdb_appender_create,      c_duckdb_appender_create_ext,      c_duckdb_appender_create_query,      c_duckdb_appender_destroy, c_duckdb_appender_begin_row, c_duckdb_append_value, c_duckdb_appender_end_row, c_duckdb_appender_flush, c_duckdb_appender_close, DuckDBValue )
import Data.Text.Foreign (withCString)
import Data.Text (Text)
import Foreign (Ptr, nullPtr, Storable (peek))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Control.Exception (finally, throwIO)
import Control.Monad (when)
import Database.DuckDB.Simple.Internal (withConnectionHandle, Connection, SQLError(..), Query(..))
import GHC.Stack (HasCallStack)
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Database.DuckDB.Simple.LogicalRep (StructField (StructField, structFieldName, structFieldValue), LogicalTypeRep)
import GHC.Generics
import Database.DuckDB.Simple.Generic (renderLogicalType)
import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Data.Text as Text
import Database.DuckDB.Simple ( duckLogicalType)
import Database.DuckDB.Simple.ToField (DuckDBColumnType, ToDuckValue (toDuckValue))

type TableName = Text

withTableAppender :: Connection -> TableName -> (DuckDBAppender -> IO a) -> IO a
withTableAppender conn tableName action = withConnectionHandle conn $ \conn' ->
    withCString tableName $ \tablePtr ->
        withAppenderAcquire
            (c_duckdb_appender_create conn' nullPtr tablePtr)
            action

withTableAppenderExt :: Connection -> TableName -> (DuckDBAppender -> IO a) -> IO a
withTableAppenderExt conn tableName action = withConnectionHandle conn $ \conn' ->
    withCString tableName $ \tablePtr ->
        withAppenderAcquire
            (c_duckdb_appender_create_ext conn' nullPtr nullPtr tablePtr)
            action

withQueryAppender :: Connection -> TableName -> [DuckDBLogicalType] -> (DuckDBAppender -> IO a) -> IO a
withQueryAppender conn query types action = withConnectionHandle conn $ \conn' ->
    withCString query $ \queryPtr ->
        withArray types $ \typeArray ->
            withAppenderAcquire
                (c_duckdb_appender_create_query conn' queryPtr (fromIntegral (length types)) typeArray nullPtr nullPtr)
                action

withAppenderAcquire :: (Ptr DuckDBAppender -> IO DuckDBState) -> (DuckDBAppender -> IO a) -> IO a
withAppenderAcquire acquire action =
    alloca $ \appPtr -> do
        state <- acquire appPtr
        when (state  /= DuckDBSuccess) $ throwIO (userError "duckdb-simple: could not acquire appender")
        case state of
          DuckDBSuccess -> pure ()
          DuckDBError -> throwIO (userError "withAppenderAcquire")
        app <- peek appPtr
        let release = do
              destroyState <- c_duckdb_appender_destroy appPtr
              when (destroyState  /= DuckDBSuccess) $ throwIO (userError "duckdb-simple: could not release appender")
            flushAndClose = do
                -- errPtr0 <- c_duckdb_appender_error_data app
                --       when (errPtr0 /= nullPtr) $ do
                --                         msg <- peekCString errPtr0
                --                         error $ toText msg
                flushState <- c_duckdb_appender_flush app
                when (flushState  /= DuckDBSuccess) $ throwIO (userError "duckdb-simple: could not release appender")
                closeState <- c_duckdb_appender_close app
                when (closeState  /= DuckDBSuccess) $ throwIO (userError "duckdb-simple: could not release appender")

        (action app <* flushAndClose) `finally` release

appendTableRow :: (HasCallStack, ToAppenderRow a) => DuckDBAppender -> a -> IO ()
appendTableRow app row = do
    assertSuccess $ c_duckdb_appender_begin_row app
    toAppenderValues row >>= mapM_ (assertSuccess . c_duckdb_append_value app)
    assertSuccess $ c_duckdb_appender_end_row app
    where
    assertSuccess :: (HasCallStack) => IO DuckDBState -> IO ()
    assertSuccess f = f >>= \case
      DuckDBSuccess -> pure ()
      _errorStatus -> throwIO $ SQLError "duckdb-simple: appendTableRow status error" Nothing Nothing -- FIXME: proper error handling

--

createTableQuery :: ToAppenderRow a => Text -> Proxy a -> Query
createTableQuery nme pxy = Query $ "CREATE TABLE \"" <> nme <> "\" (" <> tableSchema pxy <> ")" -- FIXME: unsafe

tableSchema :: ToAppenderRow a => Proxy a -> Text
tableSchema pxy = Text.intercalate ", " ["\"" <> structFieldName <> "\" " <> renderLogicalType structFieldValue | StructField{structFieldName, structFieldValue} <- toAppenderSchema pxy]

-- | Types that can be transformed into parameter bindings.
class ToAppenderRow (a :: Type) where
    toAppenderSchema :: Proxy a -> [StructField LogicalTypeRep]
    default toAppenderSchema :: (Generic a, GToAppenderRow (Rep a)) => Proxy a -> [StructField LogicalTypeRep]
    toAppenderSchema _ = gtoAppenderSchema (Proxy @(Rep a))
    toAppenderValues :: a -> IO [DuckDBValue]
    default toAppenderValues :: (Generic a, GToAppenderRow (Rep a)) => a -> IO [DuckDBValue]
    toAppenderValues = gtoAppenderValues . from

-- -- | Generic helper for deriving `ToTable`.
class GToAppenderRow (f :: Type -> Type) where
    gtoAppenderSchema :: Proxy f -> [StructField LogicalTypeRep]
    gtoAppenderValues :: f b -> IO [DuckDBValue]

instance GToAppenderRow U1 where
    gtoAppenderSchema _ = []
    gtoAppenderValues _ = pure []

instance (DuckDBColumnType a, ToDuckValue a, KnownSymbol selectorName) => GToAppenderRow (S1 ('MetaSel ('Just selectorName) q w e)(K1 i a)) where
    gtoAppenderSchema _ = [StructField ( Text.pack $ symbolVal (Proxy @selectorName)) (duckLogicalType (Proxy @a))]
    gtoAppenderValues (M1 (K1 v)) = pure <$> toDuckValue v

instance (GToAppenderRow a, GToAppenderRow b) => GToAppenderRow (a :*: b) where
    gtoAppenderSchema _ = gtoAppenderSchema (Proxy @a) ++ gtoAppenderSchema (Proxy @b)
    gtoAppenderValues (a :*: b) = gtoAppenderValues a <> gtoAppenderValues b

instance (GToAppenderRow a) => GToAppenderRow (M1 C c a) where
    gtoAppenderSchema _ = gtoAppenderSchema (Proxy @a)
    gtoAppenderValues (M1 v) = gtoAppenderValues v

instance (GToAppenderRow a) => GToAppenderRow (M1 D c a) where
    gtoAppenderSchema _ = gtoAppenderSchema (Proxy @a)
    gtoAppenderValues (M1 v) = gtoAppenderValues v
