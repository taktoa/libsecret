{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns  #-}

module Main where

import           Data.Map.Strict        (Map)
import           Data.Text              (Text)

import           Control.Monad.IO.Class (MonadIO)
import           Data.Bits              (Bits)
import           Foreign.Storable       (Storable)
import           GHC.Generics           (Generic)

import qualified Data.Text              as T
import qualified Foreign.C              as FFI
import qualified GHC.Ptr                as GHC
import qualified GI.Gio                 as G
import qualified GI.GLib                as G
import qualified GI.Secret              as S

-- Simple API

data SchemaFlag
  = SchemaNone
  | SchemaDontMatchName
  deriving (Eq, Show, Generic)

data SchemaAttr
  = SchemaAttrStr  !Text !Text
  | SchemaAttrInt  !Text !Int
  | SchemaAttrBool !Text !Bool
  deriving (Eq, Show, Generic)

schemaAttrName :: SchemaAttr -> Text
schemaAttrName (SchemaAttrStr  name _) = name
schemaAttrName (SchemaAttrInt  name _) = name
schemaAttrName (SchemaAttrBool name _) = name

getSchemaAttrStr :: SchemaAttr -> Maybe Text
getSchemaAttrStr (SchemaAttrStr _ s) = Just s
getSchemaAttrStr _                   = Nothing

getSchemaAttrInt :: SchemaAttr -> Maybe Int
getSchemaAttrInt (SchemaAttrInt _ i) = Just i
getSchemaAttrInt _                   = Nothing

getSchemaAttrBool :: SchemaAttr -> Maybe Bool
getSchemaAttrBool (SchemaAttrBool _ b) = Just b
getSchemaAttrBool _                    = Nothing

data Schema
  = MkSchema
    { schemaName  :: Text
    , schemaFlags :: [SchemaFlag]
    , schemaAttrs :: [SchemaAttr]
    }
  deriving (Eq, Show, Generic)

type Label = Text
type Password = Text
type Collection = Maybe Text
type Attributes = Map Text Text

toGSecretSchema :: Schema -> S.Schema
toGSecretSchema = _

data StoreInfo
  = MkStoreInfo
    { storeInfoSchema     :: S.Schema
    , storeInfoAttributes :: Map Text Text
    , storeInfoCollection :: Maybe Text
    }

makeStoreInfo :: Schema -> Attributes -> Collection -> StoreInfo
makeStoreInfo (toGSecretSchema -> sch) attrs coll = MkStoreInfo sch attrs coll

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
passwordStore (MkStoreInfo sch attrs coll) label pw
  = S.passwordStoreSync sch attrs coll label pw G.noCancellable

main :: IO ()
main = pure ()
