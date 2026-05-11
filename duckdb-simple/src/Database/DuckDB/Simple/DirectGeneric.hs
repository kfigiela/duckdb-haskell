{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.DuckDB.Simple.DirectGeneric
  ( DirectDuckValue (..)
  , ViaJSON(..)
  , ViaDuckStruct(..)
  , ViaDuckUnion(..)
  , ViaDuckEnum(..)
  , AppendTableRow(..)
  , DuckTypeName(..)
  )  where


import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import Data.Time (UTCTime, LocalTime (..), utcToLocalTime, utc)
import qualified Data.Text.Foreign as TextForeign

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Typeable (Typeable, TypeRep, typeRep)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Database.DuckDB.Simple.FromField (BigNum (..), IntervalValue (..), TimeWithZone (..))

import Database.DuckDB.FFI
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Aeson.Text as Aeson
import qualified Data.Text.Lazy as T
import Data.Map (Map)
import Database.DuckDB.Simple.Internal
import Data.Set (Set)
import Foreign (alloca, Storable (poke), Ptr, castPtr, withMany)
import Foreign.C.Types (CDouble (..), CFloat (CFloat))
import Foreign.Marshal.Array (withArray)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef, newIORef, readIORef)
import GHC.IO (unsafePerformIO)
import qualified Data.HashMap.Strict as HashMap
import GHC.IORef (atomicModifyIORef'_)
import Data.Foldable (toList)
import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol, symbolVal, Symbol)
import qualified Data.Aeson as A
import Database.DuckDB.Simple.ToField (ToField (toField), valueBinding)
import Data.String (IsString(..))
import Control.Monad ((>=>))
import Database.DuckDB.Simple.Internal.ValueHelpers

newtype Allocated a = Allocated {leakAllocated :: a}

newtype DuckTypeName = DuckTypeName { renderDuckTypeName :: Text }
  deriving newtype (Semigroup, Monoid, IsString)

class Destroy a where
  destroyAllocated :: Allocated a -> IO ()

instance Destroy DuckDBValue where
  destroyAllocated (Allocated a) = destroyValue a

instance Destroy DuckDBLogicalType where
  destroyAllocated (Allocated a) = destroyLogicalType a

withAllocated :: Destroy a => IO (Allocated a) -> (a -> IO b) -> IO b
withAllocated alloc go = do
  a@(Allocated val) <- alloc
  go val <* destroyAllocated a

withManyAllocated :: (Destroy a, Foldable f, Traversable f) => f (IO (Allocated a)) -> (f a -> IO b) -> IO b
withManyAllocated alloc go = do
  a <- sequence alloc
  go (leakAllocated <$> a) <* mapM_ destroyAllocated a

cache :: IORef (HashMap TypeRep (Allocated DuckDBLogicalType))
cache = unsafePerformIO (newIORef mempty)
{-# NOINLINE cache #-}

-- | We only materialize duckdb logical types once and we never release them.
-- This resulted in a significant performance gain when using high-performance appender API.
directLogicalType :: (DirectDuckValue a, Typeable a) => Proxy a -> IO (Allocated DuckDBLogicalType)
directLogicalType pxy = cacheDirectLogicalType (typeRep pxy) (directLogicalTypeUncached pxy)

cacheDirectLogicalType :: TypeRep -> IO (Allocated DuckDBLogicalType) -> IO (Allocated DuckDBLogicalType)
cacheDirectLogicalType hsRep allocate = do
  cachedMb <- HashMap.lookup hsRep <$> readIORef cache
  case cachedMb of
    Just cached -> pure cached
    Nothing -> do
      rep <- allocate
      _ <- atomicModifyIORef'_ cache (HashMap.insert hsRep rep)
      pure rep

class DirectDuckValue a where
  directDuckValue :: a -> IO (Allocated DuckDBValue)

  appendDuckValue :: DuckDBAppender -> a -> IO DuckDBState
  appendDuckValue appender val = withAllocated (directDuckValue val) (c_duckdb_append_value appender)

  directLogicalTypeUncached :: Proxy a -> IO (Allocated DuckDBLogicalType)
  directTypeName :: Proxy a -> DuckTypeName

primitiveType :: DuckDBType -> IO (Allocated DuckDBLogicalType)
primitiveType = fmap Allocated .  c_duckdb_create_logical_type

instance DirectDuckValue Bool where
    directDuckValue = fmap Allocated . boolDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeBoolean
    appendDuckValue app value = c_duckdb_append_bool app (if value then 1 else 0)
    directTypeName _ = "BOOL"

instance DirectDuckValue Int where
    directDuckValue = fmap Allocated . int64DuckValue . fromIntegral
    directLogicalTypeUncached _ = primitiveType DuckDBTypeBigInt
    appendDuckValue app = c_duckdb_append_int64 app . fromIntegral
    directTypeName _ = "INT8"

instance DirectDuckValue Int8 where
    directDuckValue = fmap Allocated . int8DuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeTinyInt
    appendDuckValue = c_duckdb_append_int8
    directTypeName _ = "INT1"

instance DirectDuckValue Int16 where
    directDuckValue = fmap Allocated . int16DuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeSmallInt
    appendDuckValue = c_duckdb_append_int16
    directTypeName _ = "INT2"

instance DirectDuckValue Int32 where
    directDuckValue = fmap Allocated . int32DuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeInteger
    appendDuckValue = c_duckdb_append_int32
    directTypeName _ = "INT4"

instance DirectDuckValue Int64 where
    directDuckValue = fmap Allocated . int64DuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeBigInt
    appendDuckValue = c_duckdb_append_int64
    directTypeName _ = "INT8"

instance DirectDuckValue Integer where
    directDuckValue = fmap Allocated .  bigNumDuckValue . BigNum
    directLogicalTypeUncached _ = primitiveType DuckDBTypeBigNum
    directTypeName _ = "BIGNUM"

instance DirectDuckValue Word where
    directDuckValue = fmap Allocated . uint64DuckValue . fromIntegral
    directLogicalTypeUncached _ = primitiveType DuckDBTypeUBigInt
    appendDuckValue app = c_duckdb_append_uint64 app . fromIntegral
    directTypeName _ = "UHUGEINT"

instance DirectDuckValue Word8 where
    directDuckValue = fmap Allocated . uint8DuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeUTinyInt
    appendDuckValue = c_duckdb_append_uint8
    directTypeName _ = "UTINYINT"

instance DirectDuckValue Word16 where
    directDuckValue = fmap Allocated . uint16DuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeUSmallInt
    appendDuckValue = c_duckdb_append_uint16
    directTypeName _ = "USMALLINT"

instance DirectDuckValue Word32 where
    directDuckValue = fmap Allocated . uint32DuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeUInteger
    appendDuckValue = c_duckdb_append_uint32
    directTypeName _ = "UINTEGER"

instance DirectDuckValue Word64 where
    directDuckValue = fmap Allocated . uint64DuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeUBigInt
    appendDuckValue = c_duckdb_append_uint64
    directTypeName _ = "UHUGEINT"

instance DirectDuckValue Float where
    directDuckValue = fmap Allocated . floatDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeFloat
    appendDuckValue app = c_duckdb_append_float app . CFloat
    directTypeName _ = "FLOAT"

instance DirectDuckValue Double where
    directDuckValue = fmap Allocated . doubleDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeDouble
    appendDuckValue app = c_duckdb_append_double app . CDouble
    directTypeName _ = "DOUBLE"

instance DirectDuckValue Text where
    directDuckValue = fmap Allocated . textDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeVarchar
    appendDuckValue app txt = TextForeign.withCString txt (c_duckdb_append_varchar app)
    directTypeName _ = "VARCHAR"

instance DirectDuckValue BS.ByteString where
    directDuckValue = fmap Allocated . blobDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeBlob
    appendDuckValue app bs = BS.useAsCStringLen bs \(ptr, len) ->
        c_duckdb_append_blob app (castPtr ptr :: Ptr ()) (fromIntegral len)
    directTypeName _ = "BLOB"

instance DirectDuckValue Day where
    directDuckValue = fmap Allocated . dayDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeDate
    appendDuckValue app = encodeDay >=> c_duckdb_append_date app
    directTypeName _ = "DATE"

instance DirectDuckValue TimeOfDay where
    directDuckValue = fmap Allocated . timeOfDayDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeTime
    appendDuckValue app = encodeTimeOfDay >=> c_duckdb_append_time app
    directTypeName _ = "TIME"

instance DirectDuckValue LocalTime where
    directDuckValue = fmap Allocated . localTimeDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeTimestamp
    appendDuckValue app = encodeLocalTime >=> c_duckdb_append_timestamp app
    directTypeName _ = "TIMESTAMP"

instance DirectDuckValue UTCTime where
    directDuckValue = fmap Allocated . utcTimeDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeTimestampTz
    appendDuckValue app = encodeLocalTime . utcToLocalTime utc >=> c_duckdb_append_timestamp app
    directTypeName _ = "TIMESTAMPTZ"

instance DirectDuckValue UUID.UUID where
    directDuckValue = fmap Allocated . uuidDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeUUID
    directTypeName _ = "UUID"

instance DirectDuckValue IntervalValue where -- TODO: Make it nominal diff time!
    directDuckValue = fmap Allocated . intervalDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeInterval
    appendDuckValue app  IntervalValue{intervalMonths, intervalDays, intervalMicros} =
        alloca \ptr -> do
            poke ptr (DuckDBInterval intervalMonths intervalDays intervalMicros)
            c_duckdb_append_interval app ptr
    directTypeName _ = "INTERVAL"

instance DirectDuckValue TimeWithZone where
    directDuckValue = fmap Allocated . timeWithZoneDuckValue
    directLogicalTypeUncached _ = primitiveType DuckDBTypeTimeTz
    -- appendDuckValue = maybe?
    directTypeName _ = "TIMETZ"

instance (DirectDuckValue a) => DirectDuckValue (Maybe a) where
    directDuckValue (Just x) = directDuckValue x
    directDuckValue Nothing = Allocated <$> nullDuckValue
    directLogicalTypeUncached _ = directLogicalTypeUncached (Proxy :: Proxy a)
    appendDuckValue app = maybe (c_duckdb_append_null app) (appendDuckValue app)
    directTypeName _ = directTypeName (Proxy @a)

-- | List values encode as DuckDB LIST (variable-length).
instance (DirectDuckValue a, Typeable a) => DirectDuckValue [a] where
    directDuckValue = arrayDuckValue
    directLogicalTypeUncached _ = do
      inner <- leakAllocated <$> directLogicalType (Proxy @a)
      Allocated <$> c_duckdb_create_list_type inner
    directTypeName _ = directTypeName (Proxy @a) <> "[]"

-- instance (DirectDuckValue a) => DirectDuckValue (Vector len a) where -- from vector sized, I don't need it right now so whatever
--     directDuckValue = arrayDuckValue
--     directLogicalTypeUncached _ = do
--       inner <- leakAllocated <$> directLogicalType (Proxy @a)
--       Allocated <$> c_duckdb_create_array_type size inner

arrayDuckValue ::
    forall a f.
    (DirectDuckValue a, Foldable f, Typeable a) =>
    f a ->
    IO (Allocated DuckDBValue)
arrayDuckValue arr = do
    elementType <- leakAllocated <$> directLogicalType (Proxy :: Proxy a)
    let elemsList = toList arr
        count = length elemsList
    withManyAllocated (directDuckValue <$> elemsList) $ \values ->
        withArray values \ptr ->
            Allocated <$> c_duckdb_create_array_value elementType ptr (fromIntegral count)


-- | NonEmpty list values encode as DuckDB LIST (variable-length).
instance (DirectDuckValue a, Typeable a) => DirectDuckValue (NonEmpty a) where
  directDuckValue = directDuckValue . toList
  directLogicalTypeUncached _ = directLogicalType (Proxy @[a])
  directTypeName _ = directTypeName (Proxy @a) <> "[]"

instance (DirectDuckValue a, Ord a, Typeable a) => DirectDuckValue (Set a) where
  directDuckValue = directDuckValue . toList
  directLogicalTypeUncached _ = directLogicalType (Proxy @[a])
  directTypeName _ = directTypeName (Proxy @a) <> "[]"

instance DirectDuckValue Aeson.Value where
  directDuckValue = fmap Allocated . textDuckValue . T.toStrict . Aeson.encodeToLazyText
  directLogicalTypeUncached _ = primitiveType DuckDBTypeVarchar -- FIXME: we need special handling for JSON
  appendDuckValue app = appendDuckValue app . T.toStrict . Aeson.encodeToLazyText
  directTypeName _ = "JSON"

-- | Map values encode as DuckDB MAP.
instance (Ord k, DirectDuckValue k, DirectDuckValue v, Typeable k, Typeable v) => DirectDuckValue (Map.Map k v) where
  directDuckValue m = do
    mapType <- leakAllocated <$> directLogicalType (Proxy @(Map k v))
    let elemsList = Map.toList m
        count = length elemsList
    withManyAllocated (directDuckValue . fst <$> elemsList) $ \keys ->
      withManyAllocated (directDuckValue . snd <$> elemsList) $ \values ->
          withArray keys \ptrK ->
          withArray values \ptrV ->
              Allocated <$> c_duckdb_create_map_value mapType ptrK ptrV (fromIntegral count)

  directLogicalTypeUncached _ = do
    kt <- leakAllocated <$> directLogicalType (Proxy @k)
    vt <- leakAllocated <$> directLogicalType (Proxy @v)
    Allocated <$> c_duckdb_create_map_type kt vt
  directTypeName _ = "MAP(" <> directTypeName (Proxy @k) <> ", " <> directTypeName (Proxy @v) <> ")"


newtype ViaDuckStruct a = ViaDuckStruct a


instance (DirectDuckValue a, Typeable a, GDuckStruct (Rep a), Generic a) => ToField (ViaDuckStruct a) where
    toField v = valueBinding "<direct>" (leakAllocated <$> directDuckValue v)

instance (Typeable a, GDuckStruct (Rep a), Generic a) => DirectDuckValue (ViaDuckStruct a) where
    directDuckValue (ViaDuckStruct v) = do
        structType <- leakAllocated <$> cacheDirectLogicalType (typeRep $ Proxy @a) (gstructTypeIO $ gstructType $ Proxy @(Rep a))
        withManyAllocated (gstructValue $ from v) $ \childValues ->
          withArray childValues $ fmap Allocated . c_duckdb_create_struct_value structType

    directLogicalTypeUncached _ = gstructTypeIO $ gstructType $ Proxy @(Rep a)

    directTypeName _ = gstructTypeName "STRUCT" $ gstructType $ Proxy @(Rep a)

data GDuckStructField = GDuckStructField { gname :: Text, gtypeName :: DuckTypeName, glogicalType :: IO (Allocated DuckDBLogicalType)}

gstructTypeIO :: [GDuckStructField] -> IO (Allocated DuckDBLogicalType)
gstructTypeIO flds = do
      evaluatedTypes <- mapM (fmap leakAllocated . glogicalType) flds
      withMany Text.withCString (gname <$> flds) $ \namePtrs ->
          withArray namePtrs $ \nameArray ->
          withArray evaluatedTypes $ \typeArray ->
                  Allocated <$> c_duckdb_create_struct_type typeArray nameArray (fromIntegral $ length flds)

gunionTypeLogical :: [GDuckStructField] -> IO (Allocated DuckDBLogicalType)
gunionTypeLogical flds = do
      evaluatedTypes <- mapM (fmap leakAllocated . glogicalType) flds
      withMany Text.withCString (gname <$> flds) $ \namePtrs ->
          withArray namePtrs $ \nameArray ->
          withArray evaluatedTypes $ \typeArray ->
                  Allocated <$> c_duckdb_create_union_type typeArray nameArray (fromIntegral $ length flds)


gstructTypeName :: Text -> [GDuckStructField] -> DuckTypeName
gstructTypeName pfx flds =   DuckTypeName $ pfx <> "(" <> Text.intercalate ", " ["\"" <> nme <> "\" " <> renderDuckTypeName tpeNme  | GDuckStructField nme tpeNme _ <- flds] <> ")"
class GDuckStruct (f :: Type -> Type) where
  gstructValue :: f b -> [IO (Allocated DuckDBValue)]
  gstructType :: Proxy f -> [GDuckStructField]

instance (DirectDuckValue a, KnownSymbol selectorName, Typeable a) => GDuckStruct (S1 ('MetaSel ('Just selectorName) q w e) (K1 i a)) where
    gstructValue (M1 (K1 v)) = pure $ directDuckValue v
    gstructType _ = pure $ GDuckStructField (Text.pack $ symbolVal (Proxy @selectorName)) (directTypeName (Proxy @a)) (directLogicalType (Proxy @a))

instance (GDuckStruct a, GDuckStruct b) => GDuckStruct (a :*: b) where
    gstructValue (a :*: b) = gstructValue a <> gstructValue b
    gstructType _ = gstructType (Proxy @a) <> gstructType (Proxy @b)

instance (GDuckStruct a) => GDuckStruct (M1 C c a) where
    gstructValue (M1 v) = gstructValue v
    gstructType _ = gstructType (Proxy @a)

instance (GDuckStruct a) => GDuckStruct (M1 D c a) where
    gstructValue (M1 v) = gstructValue v
    gstructType _ = gstructType (Proxy @a)

data TestProduct = TestProduct { foo :: Int, bar :: Day}
  deriving stock (Generic)
  deriving DirectDuckValue via (ViaDuckStruct TestProduct)

--

newtype ViaDuckUnion a = ViaDuckUnion a


instance (Typeable a, GDuckUnion (Rep a), Generic a) => DirectDuckValue (ViaDuckUnion a) where
    directDuckValue (ViaDuckUnion v) = do
        unionType <- leakAllocated <$> cacheDirectLogicalType (typeRep $ Proxy @a) (gunionTypeLogical $ gunionType $ Proxy @(Rep a))
        let (ix, valueIO) = gunionValue (Proxy @a) 0 $ from v
        withAllocated valueIO $ fmap Allocated . c_duckdb_create_union_value unionType (fromIntegral ix)
    directLogicalTypeUncached _ = gunionTypeLogical $ gunionType $ Proxy @(Rep a)

    directTypeName _ =  gstructTypeName "UNION" $ gunionType $ Proxy @(Rep a)


class GDuckUnion (f :: Type -> Type) where
  gunionValue :: Typeable a => Proxy a -> Word -> f b -> (Word, IO (Allocated DuckDBValue))
  gunionType :: Proxy f -> [GDuckStructField]

data Tople (a :: Type) (b :: Symbol)

instance (GDuckUnion a, GDuckUnion b) => GDuckUnion (a :+: b) where
  gunionValue root ix (L1 l) = gunionValue root ix l
  gunionValue root ix (R1 r) = gunionValue root (succ ix) r
  gunionType _ = gunionType (Proxy @a) <> gunionType (Proxy @b)


instance {-# OVERLAPPABLE #-} (KnownSymbol conName) => GDuckUnion (C1 ('MetaCons conName foo bar) U1) where
    gunionValue (_ :: Proxy root) ix (M1 _) = (ix, Allocated <$> nullDuckValue)
    gunionType _ = [GDuckStructField (Text.pack $ symbolVal (Proxy @conName)) "INT1" (primitiveType DuckDBTypeTinyInt)]

instance {-# OVERLAPS #-}  (GDuckStruct a, KnownSymbol conName) => GDuckUnion (C1 ('MetaCons conName foo bar) a) where
    gunionValue (_ :: Proxy root) ix (M1 v) = (ix, ) $ do
        structType <- leakAllocated <$> cacheDirectLogicalType (typeRep $ Proxy @(Tople root conName)) (gstructTypeIO $ gstructType $ Proxy @a)
        withManyAllocated (gstructValue v) $ \childValues ->
          withArray childValues $ fmap Allocated . c_duckdb_create_struct_value structType
    gunionType _ = [GDuckStructField (Text.pack $ symbolVal (Proxy @conName)) (gstructTypeName "STRUCT" t) (gstructTypeIO t)]
      where
      t = gstructType $ Proxy @a


instance (GDuckUnion a) => GDuckUnion (M1 D c a) where
    gunionValue root ix (M1 v) = gunionValue root ix v
    gunionType _ = gunionType (Proxy @a)

data TestUnion = TestUnionA { rstar :: Int, tsryutuyrsa :: UTCTime} | TestUnionB { dupa :: Int, kupa :: Day} | NoStruct
  deriving stock (Generic)
  deriving DirectDuckValue via (ViaDuckUnion TestUnion)


newtype ViaDuckEnum a = ViaDuckEnum a


instance (GDuckEnum (Rep a), Generic a, Typeable a) => DirectDuckValue (ViaDuckEnum a) where
    directDuckValue (ViaDuckEnum v) = do
        unionType <- leakAllocated <$> cacheDirectLogicalType (typeRep $ Proxy @a) (genumLogicalType $ genumType (Proxy @(Rep a)))
        let ix = genumValue 0 $ from v
        Allocated <$> c_duckdb_create_enum_value unionType (fromIntegral ix)
    directLogicalTypeUncached _ = genumLogicalType $ genumType (Proxy @(Rep a))
    directTypeName _ =  DuckTypeName $ "ENUM("  <> Text.intercalate ", " [ "\"" <> ctor <> "\"" | ctor <- genumType $ Proxy @(Rep a)] <> ")"

genumLogicalType :: [Text] -> IO (Allocated DuckDBLogicalType)
genumLogicalType els = do
      withMany Text.withCString els $ \namePtrs ->
                      withArray namePtrs \nameArray ->
                          Allocated <$> c_duckdb_create_enum_type nameArray (fromIntegral $ length els)



class GDuckEnum (f :: Type -> Type) where
  genumValue :: Word64 -> f b -> Word64
  genumType :: Proxy f -> [Text]

instance (GDuckEnum a, GDuckEnum b) => GDuckEnum (a :+: b) where
  genumValue ix (L1 l) = genumValue ix l
  genumValue ix (R1 r) = genumValue (succ ix) r
  genumType _ = genumType (Proxy @a) <> genumType (Proxy @b)


instance (KnownSymbol conName) => GDuckEnum (C1 ('MetaCons conName foo bar) U1) where
    genumValue ix (M1 _) = ix
    genumType _ = [Text.pack $ symbolVal (Proxy @conName)]

instance (GDuckEnum a) => GDuckEnum (M1 D c a) where
    genumValue ix (M1 v) = genumValue ix v
    genumType _ = genumType (Proxy @a)

data TestEnum = TestEnumA | TestEnumB | TestEnumC
  deriving stock (Generic)
  deriving DirectDuckValue via (ViaDuckEnum TestEnum)



newtype ViaJSON a = ViaJSON a


instance  A.ToJSON a => DirectDuckValue (ViaJSON a) where
    directDuckValue (ViaJSON v) = directDuckValue $ A.toJSON v
    directLogicalTypeUncached _ = directLogicalTypeUncached (Proxy @A.Value)
    directTypeName _ = directTypeName (Proxy @A.Value)
---


-- | Types that can be transformed into parameter bindings.
class AppendTableRow (a :: Type) where
  appendDuckRow :: DuckDBAppender -> a -> IO DuckDBState
  default appendDuckRow :: (Generic a, GAppendTableRow (Rep a)) => DuckDBAppender -> a -> IO DuckDBState
  appendDuckRow app = gappendDuckRow app . from

  appendDuckRowSchema :: Proxy a -> [(Text, DuckTypeName)]
  default appendDuckRowSchema :: (Generic a, GAppendTableRow (Rep a)) => Proxy a -> [(Text, DuckTypeName)]
  appendDuckRowSchema _ = gappendDuckRowSchema (Proxy @(Rep a))

class GAppendTableRow (f :: Type -> Type) where
    gappendDuckRow :: DuckDBAppender -> f b -> IO DuckDBState
    gappendDuckRowSchema :: Proxy f -> [(Text, DuckTypeName)]

instance (DirectDuckValue a, KnownSymbol selectorName) => GAppendTableRow (S1 ('MetaSel ('Just selectorName) q w e)(K1 i a)) where
    gappendDuckRowSchema _ = [( Text.pack $ symbolVal (Proxy @selectorName), directTypeName (Proxy @a))]
    gappendDuckRow app (M1 (K1 v)) = appendDuckValue app v

instance (GAppendTableRow a, GAppendTableRow b) => GAppendTableRow (a :*: b) where
    gappendDuckRowSchema _ = gappendDuckRowSchema (Proxy @a) <> gappendDuckRowSchema (Proxy @b)
    gappendDuckRow app (a :*: b) = gappendDuckRow app a >> gappendDuckRow app b

instance (GAppendTableRow a) => GAppendTableRow (M1 C c a) where
    gappendDuckRowSchema _ = gappendDuckRowSchema (Proxy @a)
    gappendDuckRow app (M1 v) = gappendDuckRow app v

instance (GAppendTableRow a) => GAppendTableRow (M1 D c a) where
    gappendDuckRowSchema _ = gappendDuckRowSchema (Proxy @a)
    gappendDuckRow app (M1 v) = gappendDuckRow app v
