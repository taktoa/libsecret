module Main where

import           Control.Monad.IO.Class

import           Data.Bits              (Bits)
import           Data.Map.Strict        (Map)

import           Data.Text              (Text)
import           Foreign.Storable       (Storable)

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
  deriving (Eq, Show)

data SchemaAttr
  = SchemaAttrStr  !Text !Text
  | SchemaAttrInt  !Text !Int
  | SchemaAttrBool !Text !Bool
  deriving (Eq, Show)

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
  deriving (Eq, Show)

passwordStore :: _
passwordStore = _

main :: IO ()
main = pure ()
