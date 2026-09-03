{-# LANGUAGE OverloadedStrings #-}

module Wayland.Decode where

import Data.Binary
import Data.Binary.Get
import Data.Bits (shiftR, (.&.))
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Text.Encoding as TE
import Wayland.Protocol
import Wayland.Types

data DecodedEvent = DecodedEvent
  { decodedEvent :: Event
  , decodedValues :: [Value]
  }

decodeInterfaceEvent :: Interface -> Opcode -> BL.ByteString -> Either DecodeError DecodedEvent
decodeInterfaceEvent iface opcode@(Opcode o) payload = do
  event <- maybe (Left $ UnknownEventOpcode o) Right $ eventAtOpcode iface opcode
  decodeEventFromBs event payload

decodeEventFromBs :: Event -> BL.ByteString -> Either DecodeError DecodedEvent
decodeEventFromBs e@Event{eventArguments = args} bs = do
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
  Right (remains, _, 0) -> Right (ValueString Nothing, remains)
  Right (remainStr, _, i) -> case runGetOrFail (getByteString (pad4 $ fromIntegral i)) remainStr of
    Left (_, _, str) -> Left $ DecodeArgFailed str
    Right (remains, _, b) -> case BS.unsnoc (BS.take (fromIntegral i) b) of
      Just (str, 0) -> Right (ValueString (Just $ decodeUtf8Lenient str), remains)
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
