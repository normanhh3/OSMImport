{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.OSMFormat where

  import Data.Int
  import Data.ProtocolBuffers
  import Data.Text
  import Data.Word
  import Data.ByteString
  import GHC.Generics (Generic)

  data BlobHeader = BlobHeader
     { bh_type :: Required 1 (Value Text)
     , bh_indexdata :: Optional 2 (Value Text)
     , bh_datasize :: Required 3 (Value Int32)
     } deriving (Generic, Show)

  instance Encode BlobHeader
  instance Decode BlobHeader

  data Blob = Blob
     { b_raw :: Optional 1 (Value ByteString) -- Not sure if this works...
     , b_raw_size :: Optional 2 (Value Int32)
     , b_zlib_data :: Optional 3 (Value ByteString)
     } deriving (Generic, Show)

  instance Encode Blob
  instance Decode Blob

  data HeaderBlock = HeaderBlock
     { hb_bbox :: Optional 1 (Message HeaderBBox)
     , hb_required_features :: Repeated 4 (Value Text)
     , hb_optional_features :: Repeated 5 (Value Text)
     , hb_writingprogram :: Optional 16 (Value Text)
     , hb_source :: Optional 17 (Value Text)
     } deriving (Generic, Show)
  
  instance Encode HeaderBlock
  instance Decode HeaderBlock

  data HeaderBBox = HeaderBBox
     { hbbox_left :: Required 1 (Value (Signed Int64))
     , hbbox_right :: Required 2 (Value (Signed Int64))
     , hbbox_top :: Required 3 (Value (Signed Int64))
     , hbbox_bottom :: Required 4 (Value (Signed Int64))
     } deriving (Generic, Show)

  instance Encode HeaderBBox
  instance Decode HeaderBBox

  data PrimitiveBlock = PrimitiveBlock
    { pb_stringtable :: Required 1 (Message StringTable)
    , pb_primitivegroup :: Repeated 2 (Message PrimitiveGroup)
    -- Granularity, units of nanodegrees, used to store coordinates in this block
    , pb_granularity :: Optional 17 (Value Int32) -- [default=100];
     -- Offset value between the output coordinates coordinates and the granularity grid in unites of nanodegrees.
    , pb_lat_offset :: Optional 19 (Value Int64) -- [default=0];
    , pb_lon_offset :: Optional 20 (Value Int64) -- [default=0];
      -- Granularity of dates, normally represented in units of milliseconds since the 1970 epoch.
    , pb_date_granularity :: Optional 18 (Value Int32) -- [default=1000];
    } deriving (Generic, Show)

  instance Encode PrimitiveBlock
  instance Decode PrimitiveBlock

  data StringTable = StringTable
    { st_bytes :: Repeated 1 (Value ByteString)
    } deriving (Generic, Show)

  instance Encode StringTable
  instance Decode StringTable

-- // Group of OSMPrimitives. All primitives in a group must be the same type.
  data PrimitiveGroup = PrimitiveGroup
    { pg_nodes :: Repeated 1 (Message Node)
    , pg_dense :: Optional 2 (Message DenseNodes)
    , pg_ways :: Repeated 3 (Message Way)
--    , pg_relations :: Repeated 4 (Message Relation)
--    , pg_change_sets :: Repeated 5 (Message ChangeSet)
    } deriving (Generic, Show)
    
  instance Encode PrimitiveGroup
  instance Decode PrimitiveGroup
    
  -- /** Optional metadata that may be included into each primitive. Special dense format used in DenseNodes. */
  data DenseInfo = DenseInfo
    { dense_info_version :: Packed 1 (Value Int32)
    , dense_info_timestamp :: Packed 2 (Value (Signed Int64))
    , dense_info_changeset :: Packed 3 (Value (Signed Int64))
    , dense_info_uid :: Packed 4 (Value (Signed Int32))
    , dense_info_user_sid :: Packed 5 (Value (Signed Int32)) -- String IDs
    } deriving (Generic, Show)

  instance Encode DenseInfo
  instance Decode DenseInfo

  data DenseNodes = DenseNodes
    { dense_nodes_id :: Packed 1 (Value (Signed Int64))
    , dense_nodes_info :: Optional 5 (Message DenseInfo)
    , dense_nodes_lat :: Packed 8 (Value (Signed Int64))
    , dense_nodes_lon :: Packed 9 (Value (Signed Int64))
    , dense_nodes_keys_vals :: Packed 10 (Value Int32)
    } deriving (Generic, Show)
    
  instance Encode DenseNodes
  instance Decode DenseNodes

  --  Optional metadata that may be included into each primitive.
  data Info = Info
    { info_version :: Optional 1 (Value Int32)
    , info_timestamp :: Optional 2 (Value Int32)
    , info_changeset :: Optional 3 (Value Int64)
    , info_uid :: Optional 4 (Value Int32)
    , info_user_sid :: Optional 5 (Value Int32)
    } deriving (Generic, Show)

  instance Encode Info
  instance Decode Info

-- Unused

-- Doesn't decompile properly, the uint32 type mostly fails with an error
  data Way = Way
    { way_id :: Required 1 (Value Int64)
    , way_keys :: Packed 2 (Value Int32) -- [packed = true];
    , way_vals :: Packed 3 (Value Int32) -- [packed = true];
    , way_info :: Optional 4 (Message Info)
    , way_refs :: Packed 8 (Value (Signed Int64)) -- [packed = true];  // DELTA coded
    --     , way_keys :: Packed 2 (Value (Fixed Word32)) -- [packed = true];
    --     , way_vals :: Packed 3 (Value (Fixed Word32)) -- [packed = true];
  } deriving (Generic, Show)

  instance Encode Way
  instance Decode Way
--
--  data Relation = Relation 
--    { relation_id :: Required 1 (Value Int64)
----    , relation_keys :: Packed 2 (Value Word32) -- [packed = true];
--    --, relation_vals :: Packed 3 (Maybe (Value Word32)) -- [packed = true];
--    -- , relation_keys :: Packed 2 (Value Int32) -- [packed = true];
--    -- , relation_vals :: Packed 3 (Value Int32) -- [packed = true];
--    , relation_info :: Optional 4 (Message Info)
--    , relation_roles_sid :: Packed 8 (Value Int32) -- [packed = true];
--    , relation_memids :: Packed 9 (Value (Signed Int64)) -- [packed = true];
--    } deriving (Generic, Show)
--  -- //Need to add member types
--  instance Encode Relation
--  instance Decode Relation

  data Node = Node
    { node_id :: Required 1 (Value (Signed Int64))
    , node_keys :: Packed 2 (Value Int32) -- [packed = true]; // String IDs.
    , node_vals :: Packed 3 (Value Int32) -- [packed = true]; // String IDs.
    , node_info :: Optional 4 (Message Info)
    , node_lat :: Required 8 (Value Int64)
    , node_lon :: Required 9 (Value Int64)
    } deriving (Generic, Show)

  instance Encode Node
  instance Decode Node

-- // TODO: REMOVE THIS? NOT in osmosis schema.
  data ChangeSet = ChangeSet
    { change_set_id :: Required 1 (Value Int64)
    -- Parallel arrays.
    , change_set_keys :: Packed 2 (Value Int32) -- [packed = true]; // String IDs.
    , change_set_vals :: Packed 3 (Value Int32) -- [packed = true]; // String IDs.
    , change_set_info :: Optional 4 (Message Info)
    , change_set_created_at :: Required 8 (Value Int64)
    , change_set_closetime_delta :: Optional 9 (Value Int64)
    , change_set_open :: Required 10 (Value Int64)
    , change_set_bbox :: Optional 10 (Message HeaderBBox)
    } deriving (Generic, Show)

  instance Encode ChangeSet
  instance Decode ChangeSet


