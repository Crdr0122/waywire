module Wayland.Types where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Word

type Fixed = Int32

data NewObject -- TODO Placeholder for new_id

data ObjectId = ObjectId Word32 deriving (Eq, Ord, Show)
data Opcode = Opcode Word16 deriving (Eq, Ord, Show)

data Object a = Object
  { objectId :: ObjectId
  , objectConnection :: Connection
  }
  deriving (Eq, Show)

data Value
  = ValueInt Int32
  | ValueUInt Word32
  | ValueFixed Fixed
  | ValueString (Maybe Text)
  | ValueArray ByteString
  | ValueObject ObjectId
  | ValueNewId ObjectId
  | ValueFd
  deriving (Show)

data ValueType
  = ValueTypeInt
  | ValueTypeUInt
  | ValueTypeFixed
  | ValueTypeString
  | ValueTypeArray
  | ValueTypeObject
  | ValueTypeNewId
  | ValueTypeFd

data Connection = Connection deriving (Eq, Show)
