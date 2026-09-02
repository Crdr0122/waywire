{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wayland where

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
  print $ objectId display

display = Object (ObjectId 1) Connection
