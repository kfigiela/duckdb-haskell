{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Database.DuckDB.Simple.Appender
Description : High-performance appender API
-}
module Database.DuckDB.Simple.Appender
  ( withTableAppender
  , withTableAppenderExt
  , withQueryAppender
  )
  where

import Database.DuckDB.FFI
    ( DuckDBAppender,
      DuckDBLogicalType,
      DuckDBState,
      pattern DuckDBSuccess,
      pattern DuckDBError,
      c_duckdb_appender_create,
      c_duckdb_appender_create_ext,
      c_duckdb_appender_create_query,
      c_duckdb_appender_destroy )
import Data.Text.Foreign (withCString)
import Data.Text (Text)
import Foreign (Ptr, nullPtr, Storable (peek))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Control.Exception (finally, throwIO)
import Control.Monad (when)
import Database.DuckDB.Simple.Internal (withConnectionHandle, Connection)

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
          DuckDBError -> error "withAppenderAcquire"
        app <- peek appPtr
        let release = do
              destroyState <- c_duckdb_appender_destroy appPtr
              when (destroyState  /= DuckDBSuccess) $ throwIO (userError "duckdb-simple: could not release appender")
        action app `finally` release
