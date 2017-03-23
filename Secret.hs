{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Category
import           Control.Monad

import           Control.Monad.Ether    as Ether

import           Data.Monoid

import           Data.Map.Strict        (Map)
import           Data.Text              (Text)

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bits              (Bits)
import           Foreign.Storable       (Storable)
import           GHC.Generics           (Generic)

import qualified DBus

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Foreign.C              as FFI
import qualified GHC.Ptr                as GHC

import qualified Data.GI.Base           as G
import qualified GI.Gio                 as G
import qualified GI.GLib                as G
import qualified GI.Secret              as S

import           GI.Secret              (AttrOp ((:=)))

import qualified DBus                   as Exported (parseObjectPath)

data ObjectPathError

getObjectPath :: forall m.
                 ( MonadIO m
                 , MonadExcept "" ObjectPathError m
                 ) => S.Collection -> m DBus.ObjectPath
getObjectPath = G.getDBusProxyGObjectPath >=> parseOP >>> liftIO
  where
    parseOP :: Maybe Text -> IO DBus.ObjectPath
    parseOP mt = maybe parseFailure pure
                 (mt >>= (T.unpack >>> DBus.parseObjectPath))

-- Simple API

-- | FIXME: doc
data SchemaFlag
  = SFNone
    -- ^ FIXME: doc
  | SFDontMatchName
    -- ^ FIXME: doc
  deriving (Eq, Show, Generic)

data SchemaAttrType
  = SchemaAttrTypeStr
  | SchemaAttrTypeInt
  | SchemaAttrTypeBool
  deriving (Eq, Show, Generic)

data SchemaAttr
  = MkSchemaAttr
    { schemaAttrName :: !Text
    , schemaAttrType :: !SchemaAttrType
    }
  deriving (Eq, Show, Generic)

newtype GISchemaAttr = MkGISchemaAttr (BS.ByteString, S.SchemaAttribute)

toGISchemaAttr = _

-- | FIXME: doc
data Schema
  = MkSchema
    { schemaName  :: Text
      -- ^ FIXME: doc
    , schemaFlags :: [SchemaFlag]
      -- ^ FIXME: doc
    }
  deriving (Eq, Show, Generic)

-- Matches the lifetime of the Schema with the lifetime of its name attribute.
newtype GISchema = MkGISchema (BS.ByteString, S.Schema)

toGISchema :: (MonadIO m) => Schema -> m GISchema
toGISchema sch = liftIO $ do
  let name = T.encodeUtf8 (schemaName sch) <> "\0"
  nameCS <- BS.unsafeUseAsCString name pure
  let fromSF SFNone          = S.SchemaFlagsNone
      fromSF SFDontMatchName = S.SchemaFlagsDontMatchName
  let flags = map fromSF (schemaFlags sch)
  giSchema <- G.new S.Schema [S.schema_name := nameCS, S.schema_flags := flags]
  pure $ MkGISchema (name, giSchema)

withGISchema :: (MonadIO m) => GISchema -> (S.Schema -> m a) -> m a
withGISchema (MkGISchema (_, sch)) cb = cb sch

--------------------------------------------------------------------------------

-- | FIXME: doc
data Collection
  = CollectionDefault
    -- ^ The default collection.
  | CollectionSession
    -- ^ The "session" collection; discards passwords across login sessions.
  | CollectionDBus !DBus.ObjectPath
    -- ^ The DBus object path of a collection in which to store the secret.
  deriving (Eq, Show, Generic)

type GICollection = Maybe Text

toGICollection :: Collection -> GICollection
toGICollection CollectionDefault   = Just S.COLLECTION_DEFAULT
toGICollection CollectionSession   = Just S.COLLECTION_SESSION
toGICollection (CollectionDBus ob) = Just (T.pack (DBus.formatObjectPath ob))

--------------------------------------------------------------------------------

type Label = Text
type Password = Text


type Attributes = Map Text Text

-- | FIXME: doc
data StoreInfo
  = MkStoreInfo
    { storeInfoSchema     :: GISchema
    , storeInfoAttributes :: Map Text Text
    , storeInfoCollection :: Maybe Text
    }

-- | FIXME: doc
mkStoreInfo :: (MonadIO m) => Schema -> Attributes -> Collection -> m StoreInfo
mkStoreInfo sch attrs coll = MkStoreInfo
                             <$> toGISchema sch
                             <*> pure attrs
                             <*> pure (toGICollection coll)

-- | Store a password with libsecret.
--
-- FIXME: more docs
--
-- Note that this function is synchronous; if you want asynchrony, wrap this
-- call with 'Control.Concurrent.Async.async'.
passwordStore :: (MonadIO io)
              => StoreInfo
              -> Label
              -> Password
              -> io ()
passwordStore (MkStoreInfo schema attrs coll) label pw
  = withGISchema schema
    (\sch -> S.passwordStoreSync sch attrs coll label pw G.noCancellable)

main :: IO ()
main = pure ()
