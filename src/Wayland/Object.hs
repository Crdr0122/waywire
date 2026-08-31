module Wayland.Object where

import Data.ByteString as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.Text as T
import Data.Text.Encoding as TE
import Data.Word
import System.Posix.Types
import Wayland.Protocol

data NewObject -- TODO Placeholder for new_id

data ObjectId = ObjectId Word32 deriving (Eq, Ord, Show)
data Opcode = Opcode Word16 deriving (Eq, Ord, Show)

data Object a = Object
  { objectId :: ObjectId
  , objectConnection :: Connection
  }
  deriving (Eq, Show)

data Connection = Connection deriving (Eq, Show)

data Value
  = ValueInt Int32
  | ValueUInt Word32
  | ValueFixed Fixed
  | ValueString Text
  | ValueNullString
  | ValueArray ByteString
  | ValueObject ObjectId
  | ValueNewId ObjectId
  | ValueFd Fd

data Message = Message
  { messageObject :: ObjectId
  , messageOpcode :: Opcode
  , messagePayload :: [Value]
  }

data DecodeError
  = NotEnoughBytes
  | InvalidMessageSize Word32
  | MessageTruncated
  deriving (Eq, Show)

encodeMessage :: Message -> ByteString
encodeMessage (Message (ObjectId objId) (Opcode op) payload) =
  BL.toStrict . B.toLazyByteString $ headerBuilder <> payloadBuilder
 where
  payloadSizes = valueSize <$> payload
  totalPayloadSize = sum payloadSizes
  totalMsgSize = 8 + totalPayloadSize

  -- 2. Build 8-byte Header:
  --    [4 bytes: Object ID] [2 bytes: Opcode] [2 bytes: Total Size]
  headerBuilder =
    B.word32LE objId
      <> B.word16LE (fromIntegral totalMsgSize)
      <> B.word16LE op

  -- 3. Build Payload
  payloadBuilder = foldMap encodeValue payload

valueSize :: Value -> Int
valueSize (ValueInt _) = 4
valueSize (ValueUInt _) = 4
valueSize (ValueFixed _) = 4
valueSize (ValueObject _) = 4
valueSize (ValueNewId _) = 4
valueSize (ValueFd _) = 0
valueSize (ValueString txt) =
  let len = BS.length (TE.encodeUtf8 txt) + 1 -- includes NUL terminator
   in 4 + pad4 len
valueSize ValueNullString = 4
valueSize (ValueArray bs) =
  let len = BS.length bs
   in 4 + pad4 len

encodeValue :: Value -> B.Builder
encodeValue (ValueInt i) = B.int32LE i
encodeValue (ValueUInt u) = B.word32LE u
encodeValue (ValueFixed f) = B.int32LE f
encodeValue (ValueObject (ObjectId o)) = B.word32LE o
encodeValue (ValueNewId (ObjectId n)) = B.word32LE n
encodeValue (ValueFd _) = mempty
encodeValue ValueNullString = B.word32LE 0
encodeValue (ValueString txt) =
  let bs = TE.encodeUtf8 txt
      len = BS.length bs + 1 -- include NUL
      padLen = pad4 len - len
   in B.word32LE (fromIntegral len)
        <> B.byteString bs
        <> B.word8 0 -- NUL terminator
        <> B.byteString (BS.replicate padLen 0)
encodeValue (ValueArray bs) =
  let len = BS.length bs
      padLen = pad4 len - len
   in B.word32LE (fromIntegral len)
        <> B.byteString bs
        <> B.byteString (BS.replicate padLen 0)

pad4 :: Int -> Int
pad4 n = (n + 3) `div` 4 * 4
