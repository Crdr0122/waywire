{-# LANGUAGE OverloadedStrings #-}

module Wayland.Encode where

import Data.ByteString as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding as TE
import Wayland.Types

encodeMessage :: Message -> BL.ByteString
encodeMessage (Message (ObjectId objId) (Opcode op) payload _) =
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
valueSize (ValueFd _) = 0
valueSize (ValueString (Just txt)) =
  let len = BS.length (TE.encodeUtf8 txt) + 1 -- includes NUL terminator
   in 4 + pad4 len
valueSize (ValueString Nothing) = 4
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
encodeValue (ValueString Nothing) = B.word32LE 0
encodeValue (ValueString (Just txt)) =
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
