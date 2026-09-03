{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wayland where

import Data.Int
import Language.Haskell.TH
import Text.XML
import Text.XML.Cursor
import Wayland.Encode
import Wayland.Protocol
import Wayland.Protocol.Parser
import Wayland.TH
import Wayland.Types

$(testProtocol)

main :: IO ()
main = do
  putStrLn ""
  print $ unObject display

display :: Object WlDisplay
display = Object (ObjectId 1)

encodeWlDisplaySyncRequest :: Object WlDisplay -> Int32 -> Object WlCallback -> Message
encodeWlDisplaySyncRequest display testInt callback =
  Message
    (unObject display) -- Use the actual object ID
    (Opcode 0)
    [ValueNewId (unObject callback), ValueInt testInt]
