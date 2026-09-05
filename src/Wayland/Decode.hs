{-# LANGUAGE OverloadedStrings #-}

module Wayland.Decode where

import Data.Binary
import Data.Binary.Get
import Data.Bits (shiftR, (.&.))
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding as TE
import System.Posix.Types (Fd)
import Wayland.Protocol
import Wayland.Types

argTypeToValueType :: ArgType -> ValueType
argTypeToValueType TypeInt = ValueTypeInt
argTypeToValueType TypeUInt = ValueTypeUInt
argTypeToValueType TypeFixed = ValueTypeFixed
argTypeToValueType (TypeString _) = ValueTypeString
argTypeToValueType TypeArray = ValueTypeArray
argTypeToValueType (TypeObject{}) = ValueTypeObject
argTypeToValueType (TypeNewId{}) = ValueTypeNewId
argTypeToValueType TypeFileDescriptor = ValueTypeFd

decodeMessageHeader :: BL.ByteString -> Either DecodeError (ObjectId, Opcode, BL.ByteString, BL.ByteString)
decodeMessageHeader bs
  | l < 8 = Left NotEnoughBytes
  | otherwise = case runGetOrFail getHeader bs of
      Left (_, _, str) -> Left $ DecodeHeaderFailed str
      Right (_, _, (obId, sizeOpcode))
        | size < 8 -> Left $ InvalidMessageSize size
        | (fromIntegral size) > l -> Left NotEnoughBytes
        | otherwise -> Right (obId, code, body, rest)
       where
        size :: Word16
        size = fromIntegral $ sizeOpcode `shiftR` 16
        code :: Opcode
        code = Opcode $ fromIntegral $ sizeOpcode .&. 0xFFFF
        body = BL.take (fromIntegral size - 8) (BL.drop 8 bs)
        rest = BL.drop (fromIntegral size) bs
 where
  l = BL.length bs

{- | Decodes a message's arguments AND threads the connection's fd
queue through: an 'fd'-typed argument consumes zero *bytes* but one
fd off the front of the supplied list, and the leftover fds (not
needed by this message) are handed back for the next one. Running
out of fds is reported the same way as running out of bytes
('NotEnoughFds') -- it means "the sendmsg() call carrying them
hasn't arrived yet," not "the message is malformed."
-}
decodeValues :: [ValueType] -> [Fd] -> BL.ByteString -> Either DecodeError ([Value], [Fd])
decodeValues (ValueTypeFd : ts) fds bs = case fds of
  [] -> Left NotEnoughFds
  (fd : restFds) -> do
    (values, leftoverFds) <- decodeValues ts restFds bs
    pure (ValueFd fd : values, leftoverFds)
decodeValues (t : ts) fds bs = do
  (value, remains) <- decodeValue t bs
  (values, leftoverFds) <- decodeValues ts fds remains
  pure (value : values, leftoverFds)
decodeValues [] fds bs
  | BL.null bs = Right ([], fds)
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
decodeValue ValueTypeFd _ = Left UnexpectedFdArg
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
