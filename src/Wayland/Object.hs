{-# LANGUAGE OverloadedStrings #-}

module Wayland.Object where

import Data.Binary
import Data.Binary.Get
import Data.Bits (shiftR, (.&.))
import Data.ByteString as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.List qualified as L
import Data.Text as T
import Data.Text.Encoding as TE
import Data.Word
import System.Posix.Types
import Wayland.Protocol
import Wayland.Types

data Value
  = ValueInt Int32
  | ValueUInt Word32
  | ValueFixed Fixed
  | ValueString Text
  | ValueNullString
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

data Message = Message
  { messageObject :: ObjectId
  , messageOpcode :: Opcode
  , messagePayload :: [Value]
  }
  deriving (Show)

data DecodeError
  = NotEnoughBytes
  | InvalidMessageSize Word16
  | MessageTruncated
  | DecodeHeaderFailed String
  | DecodeArgFailed String
  | InvalidString
  | UnknownEventOpcode Word16
  | ExtraBytes
  deriving (Eq, Show)

data DecodedEvent = DecodedEvent
  { decodedEvent :: Event
  , decodedValues :: [Value]
  }

decodeInterfaceEvent :: Interface -> Opcode -> BL.ByteString -> Either DecodeError DecodedEvent
decodeInterfaceEvent iface opcode@(Opcode o) payload = do
  event <- maybe (Left $ UnknownEventOpcode o) Right $ eventAtOpcode iface opcode
  decodeEvent event payload

decodeEvent :: Event -> BL.ByteString -> Either DecodeError DecodedEvent
decodeEvent e@Event{eventArguments = args} bs = do
  values <- decodeValues types bs
  pure $ DecodedEvent e values
 where
  types = argTypeToValueType . argType <$> args

eventAtOpcode :: Interface -> Opcode -> Maybe Event
eventAtOpcode Interface{ifaceEvents = e} (Opcode o) = e L.!? (fromIntegral o)

requestAtOpcode :: Interface -> Opcode -> Maybe Request
requestAtOpcode Interface{ifaceRequests = r} (Opcode o) = r L.!? (fromIntegral o)

testMessage :: Message
testMessage =
  Message
    (ObjectId 3)
    (Opcode 2)
    [ ValueUInt 42
    , ValueString "hello"
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

argTypeToValueType :: ArgType -> ValueType
argTypeToValueType TypeInt = ValueTypeInt
argTypeToValueType TypeUInt = ValueTypeUInt
argTypeToValueType TypeFixed = ValueTypeFixed
argTypeToValueType (TypeString _) = ValueTypeString
argTypeToValueType TypeArray = ValueTypeArray
argTypeToValueType (TypeObject{}) = ValueTypeObject
argTypeToValueType (TypeNewId{}) = ValueTypeNewId
argTypeToValueType TypeFileDescriptor = ValueTypeFd

decodeMessageHeader :: BL.ByteString -> Either DecodeError (ObjectId, Opcode, BL.ByteString)
decodeMessageHeader bs
  | l < 8 = Left NotEnoughBytes
  | otherwise = case runGetOrFail getHeader bs of
      Left (_, _, str) -> Left $ DecodeHeaderFailed str
      Right (_, _, (obId, sizeOpcode))
        | size < 8 -> Left $ InvalidMessageSize size
        | (fromIntegral size) > l -> Left MessageTruncated
        | otherwise -> Right (obId, code, BL.take (fromIntegral size - 8) (BL.drop 8 bs))
       where
        size :: Word16
        size = fromIntegral $ sizeOpcode `shiftR` 16
        code :: Opcode
        code = Opcode $ fromIntegral $ sizeOpcode .&. 0xFFFF
 where
  l = BL.length bs

decodeValues :: [ValueType] -> BL.ByteString -> Either DecodeError [Value]
decodeValues (t : ts) bs = do
  (value, remains) <- decodeValue t bs
  values <- decodeValues ts remains
  pure (value : values)
decodeValues [] bs
  | BL.null bs = Right []
  | otherwise = Left ExtraBytes

decodeValue :: ValueType -> BL.ByteString -> Either DecodeError (Value, BL.ByteString)
decodeValue ValueTypeInt bs = do
  (i, remains) <- runDecoder getInt32le bs
  pure (ValueInt i, remains)
decodeValue ValueTypeUInt bs = do
  (i, remains) <- runDecoder getWord32le bs
  pure (ValueUInt i, remains)
decodeValue ValueTypeFixed bs = do
  (i, remains) <- runDecoder getInt32le bs
  pure (ValueFixed i, remains)
decodeValue ValueTypeObject bs = do
  (i, remains) <- runDecoder getWord32le bs
  pure (ValueObject (ObjectId i), remains)
decodeValue ValueTypeNewId bs = do
  (i, remains) <- runDecoder getWord32le bs
  pure (ValueNewId (ObjectId i), remains)
decodeValue ValueTypeFd bs = pure (ValueFd, bs)
decodeValue ValueTypeArray bs = case runGetOrFail getWord32le bs of
  Left (_, _, str) -> Left $ DecodeArgFailed str
  Right (remainArray, _, i) -> case runGetOrFail (getByteString (pad4 $ fromIntegral i)) remainArray of
    Left (_, _, str) -> Left $ DecodeArgFailed str
    Right (remains, _, arr) -> Right (ValueArray (BS.take (fromIntegral i) arr), remains)
decodeValue ValueTypeString bs = case runGetOrFail getWord32le bs of
  Left (_, _, str) -> Left $ DecodeArgFailed str
  Right (remains, _, 0) -> Right (ValueNullString, remains)
  Right (remainStr, _, i) -> case runGetOrFail (getByteString (pad4 $ fromIntegral i)) remainStr of
    Left (_, _, str) -> Left $ DecodeArgFailed str
    Right (remains, _, b) -> case BS.unsnoc (BS.take (fromIntegral i) b) of
      Just (str, 0) -> Right (ValueString (decodeUtf8Lenient str), remains)
      _ -> Left InvalidString

runDecoder :: Get a -> BL.ByteString -> Either DecodeError (a, BL.ByteString)
runDecoder getter bs = case runGetOrFail getter bs of
  Left (_, _, err) -> Left $ DecodeArgFailed err
  Right (remains, _, value) -> Right (value, remains)

getHeader :: Get (ObjectId, Word32)
getHeader = do
  objectId <- ObjectId <$> getWord32le
  header <- getWord32le
  pure (objectId, header)

encodeMessage :: Message -> BL.ByteString
encodeMessage (Message (ObjectId objId) (Opcode op) payload) =
  B.toLazyByteString $ headerBuilder <> payloadBuilder
 where
  payloadSizes = valueSize <$> payload
  totalPayloadSize = sum payloadSizes
  totalMsgSize = 8 + totalPayloadSize

  headerBuilder =
    B.word32LE objId
      <> B.word16LE op
      <> B.word16LE (fromIntegral totalMsgSize)

  payloadBuilder = foldMap encodeValue payload

valueSize :: Value -> Int
valueSize (ValueInt _) = 4
valueSize (ValueUInt _) = 4
valueSize (ValueFixed _) = 4
valueSize (ValueObject _) = 4
valueSize (ValueNewId _) = 4
valueSize ValueFd = 0
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
encodeValue ValueFd = mempty
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
