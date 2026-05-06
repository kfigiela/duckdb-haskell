{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Database.DuckDB.Simple.ToRow
Description : Rendering Haskell collections into parameter rows.

The `ToRow` class converts Haskell values into lists of `FieldBinding`s that
can be consumed by statement binding functions.
-}
module Database.DuckDB.Simple.ToRow (
    ToRow (..),
    GToRow (..),
) where

import Database.DuckDB.Simple.ToField (FieldBinding, ToField (..))
import Database.DuckDB.Simple.Types (Only (..), (:.) (..))
import Database.DuckDB.FFI (DuckDBValue)

import GHC.Generics

-- | Types that can be transformed into parameter bindings.
class ToRow a where
    toRow :: a -> [FieldBinding]
    default toRow :: (Generic a, GToRow (Rep a)) => a -> [FieldBinding]
    toRow = gtoRow . from

    toRowValue :: a -> IO [DuckDBValue]
    default toRowValue :: (Generic a, GToRow (Rep a)) => a -> IO [DuckDBValue]
    toRowValue = gtoRowValue . from

-- | Generic helper for deriving `ToRow`.
class GToRow f where
    gtoRow :: f p -> [FieldBinding]
    gtoRowValue :: f p -> IO [DuckDBValue]


instance GToRow U1 where
    gtoRow _ = []
    gtoRowValue _ = pure []

instance (ToField a) => GToRow (K1 i a) where
    gtoRow (K1 a) = [toField a]
    gtoRowValue (K1 a) = pure <$> toFieldValue a

instance (GToRow a, GToRow b) => GToRow (a :*: b) where
    gtoRow (a :*: b) = gtoRow a ++ gtoRow b
    gtoRowValue (a :*: b) = gtoRowValue a <> gtoRowValue b

instance (GToRow a) => GToRow (M1 i c a) where
    gtoRow (M1 a) = gtoRow a
    gtoRowValue (M1 a) = gtoRowValue a

instance ToRow () where
    toRow () = []
    toRowValue () = pure []

instance (ToField a) => ToRow (Only a) where
    toRow (Only a) = [toField a]
    toRowValue (Only a) = pure <$> toFieldValue a

instance (ToField a, ToField b) => ToRow (a, b) where
    toRow (a, b) = [toField a, toField b]
    toRowValue (a, b) = sequence [toFieldValue a, toFieldValue b]

instance (ToField a, ToField b, ToField c) => ToRow (a, b, c) where
    toRow (a, b, c) = [toField a, toField b, toField c]
    toRowValue (a, b, c) = sequence [toFieldValue a, toFieldValue b, toFieldValue c]

instance (ToField a, ToField b, ToField c, ToField d) => ToRow (a, b, c, d) where
    toRow (a, b, c, d) = [toField a, toField b, toField c, toField d]
    toRowValue (a, b, c, d) = sequence [toFieldValue a, toFieldValue b, toFieldValue c, toFieldValue d]

instance (ToField a, ToField b, ToField c, ToField d, ToField e) => ToRow (a, b, c, d, e) where
    toRow (a, b, c, d, e) = [toField a, toField b, toField c, toField d, toField e]
    toRowValue (a, b, c, d, e) = sequence [toFieldValue a, toFieldValue b, toFieldValue c, toFieldValue d, toFieldValue e]

instance (ToField a) => ToRow [a] where
    toRow = fmap toField
    toRowValue = mapM toFieldValue

instance (ToRow a, ToRow b) => ToRow (a :. b) where
    toRow (a :. b) = toRow a ++ toRow b
    toRowValue (a :. b) = toRowValue a <> toRowValue b
