{-# LANGUAGE OverloadedStrings #-}

module Wayland.Types where

import Data.ByteString (ByteString)
import Data.Int
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable
import Data.Word
import Network.Socket

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

type ObjectRegistry = Map ObjectId InterfaceType

data SomeEvent where
  SomeEvent :: (Typeable a, Show a) => a -> SomeEvent
data SomeRequest where
  SomeRequest :: (Typeable a, Show a) => a -> SomeRequest

data InterfaceType = InterfaceType
  { interfaceName :: Text
  , interfaceVersion :: Int
  , interfaceDecodeEvent :: Opcode -> [Value] -> Either DecodeError SomeEvent
  , interfaceEncodeRequest :: SomeRequest -> ByteString
  }

data Connection = Connection
  { connSocket :: Socket
  , connRegistry :: ObjectRegistry
  , connNextObjectId :: Word32  -- For allocating new IDs
  , connPendingObjects :: Map ObjectId InterfaceType
  }

allocateNewId :: Connection -> (ObjectId, Connection)
allocateNewId conn = 
  let newId = connNextObjectId conn
      conn' = conn { connNextObjectId = newId + 1 }
  in (ObjectId newId, conn')

pad4 :: Int -> Int
pad4 n = (n + 3) `div` 4 * 4

testMessage :: Message
testMessage =
  Message
    (ObjectId 3)
    (Opcode 2)
    [ ValueUInt 42
    , ValueString (Just "hello")
    , ValueObject (ObjectId 7)
    , ValueArray "abc"
    ]

testTypes :: [ValueType]
testTypes =
  [ ValueTypeUInt
  , ValueTypeString
  , ValueTypeObject
  , ValueTypeArray
  ]
