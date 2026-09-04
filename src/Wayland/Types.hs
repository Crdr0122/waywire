{-# LANGUAGE OverloadedStrings #-}

module Wayland.Types where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.Map (Map, insert)
import Data.Text (Text)
import Data.Typeable
import Data.Word
import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll)
import System.Posix.Types (Fd)

type Fixed = Int32

data NewObject -- TODO Placeholder for new_id

newtype ObjectId = ObjectId Word32 deriving (Eq, Ord, Show)
newtype Opcode = Opcode Word16 deriving (Eq, Ord, Show)

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
  | ValueFd Fd
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
  , messageFd :: [Fd]
  }
  deriving (Show)

data SomeEvent where
  SomeEvent :: (Typeable a, Show a) => a -> SomeEvent

data InterfaceType = InterfaceType
  { interfaceName :: Text
  , interfaceVersion :: Int
  , interfaceDecodeEvent :: Opcode -> [Value] -> Either DecodeError SomeEvent
  }

data Env = Env
  { envRegistry :: MVar (Map ObjectId ObjectEntry)
  , envIdAlloc :: MVar Word32
  , envSocket :: MVar Socket
  }

newtype ObjectEntry = ObjectEntry
  {dispatchEvent :: Opcode -> ByteString -> Either String (W ())}

type W a = ReaderT Env IO a

allocateNewId :: W ObjectId
allocateNewId = do
  i <- asks envIdAlloc
  liftIO $ modifyMVar i (\x -> pure (x + 1, ObjectId (x + 1)))

sendMessage :: BL.ByteString -> W ()
sendMessage msg = do
  s <- asks envSocket
  liftIO $ withMVar s (`sendAll` msg)

registerObject :: ObjectId -> ObjectEntry -> W ()
registerObject oid entry = do
  ref <- asks envRegistry
  liftIO $ (modifyMVar_ ref $ pure . insert oid entry)

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
    []

testTypes :: [ValueType]
testTypes =
  [ ValueTypeUInt
  , ValueTypeString
  , ValueTypeObject
  , ValueTypeArray
  ]
