{-# LANGUAGE TypeFamilies #-}

module Wayland.Types where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Word

type Fixed = Int32

data NewObject -- TODO Placeholder for new_id

data ObjectId = ObjectId Word32 deriving (Eq, Ord, Show)
data Opcode = Opcode Word16 deriving (Eq, Ord, Show)

newtype Object a = Object
  { unObject :: ObjectId
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

data DecodeError
  = NotEnoughBytes
  | InvalidMessageSize Word16
  | MessageTruncated
  | DecodeHeaderFailed String
  | DecodeArgFailed String
  | InvalidString
  | UnknownEventOpcode Word16
  | ValueToEventFailure
  | ExtraBytes
  deriving (Eq, Show)

data Message = Message
  { messageObject :: ObjectId
  , messageOpcode :: Opcode
  , messagePayload :: [Value]
  }
  deriving (Show)

pad4 :: Int -> Int
pad4 n = (n + 3) `div` 4 * 4
